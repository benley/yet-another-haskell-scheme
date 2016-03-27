#!/usr/bin/env runhaskell
{-# LANGUAGE ExistentialQuantification #-}

module Scheme where

import Control.Monad
import Control.Monad.Except
import Data.IORef
import Data.Maybe (isJust, isNothing)
import System.Console.Haskeline
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

import Yahs.Util

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (NumArgs expected found)      = "Expected " ++ show expected
                                          ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                          ++ ", found " ++ show found

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

data LispVal = Atom          String
             | List          [LispVal]
             | DottedList    [LispVal] LispVal
             | Number        Integer
             | String        String
             | Bool          Bool
             | Character     Char
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc        ([LispVal] -> IOThrowsError LispVal)
             | Port          Handle
             | Func          { params  :: [String]
                             , vararg  :: (Maybe String)
                             , body    :: [LispVal]
                             , closure :: Env
                             }

instance Show LispVal where show = showVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escapedChar <|> noneOf "\"")
    char '"'
    return (String x)

escapedChar :: Parser Char
escapedChar = do
    char '\\'
    x <- oneOf "nrt\\\""
    return $ case x of
               'n'  -> '\n'
               'r'  -> '\r'
               't'  -> '\t'
               '\\' -> '\\'
               '"'  -> '"'

namedCharacter :: Parser Char
namedCharacter = do
    x <- string "space" <|> string "newline"
    return $ case x of
               "space"   -> ' '
               "newline" -> '\n'

parseChar :: Parser LispVal
parseChar = try (string "#\\" >> (namedCharacter <|> anyChar)) >>= return . Character

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
               "#t" -> Bool True
               "#f" -> Bool False
               _    -> Atom atom

binaryRadixNumber :: Parser LispVal
binaryRadixNumber = do
    num <- try (string "#b" >> many1 (oneOf "01"))
    notFollowedBy digit
    return $ Number (bin2dec num)

bin2dec :: String -> Integer
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
          where c2i c = if c == '0' then 0 else 1

decimalNumber :: Parser LispVal
decimalNumber =
    try $ optional (string "#d") >>
    (negativeDecimalNumber <|> ((Number . read) <$> many1 digit))

negativeDecimalNumber :: Parser LispVal
negativeDecimalNumber = try (char '-' >> Number . read . ('-' :) <$> many1 digit)

hexRadixNumber :: Parser LispVal
hexRadixNumber = do
    num <- try (string "#x" >> many1 hexDigit)
    notFollowedBy letter
    return $ Number $ read $ "0x" ++ num

octalRadixNumber :: Parser LispVal
octalRadixNumber = do
    num <- try (string "#o" >> (many1 $ octDigit))
    notFollowedBy digit
    return $ Number $ read $ "0o" ++ num

parseNumber :: Parser LispVal
parseNumber = decimalNumber
              <|> binaryRadixNumber
              <|> hexRadixNumber
              <|> octalRadixNumber

parseQuoted :: Parser LispVal
parseQuoted = do
    x <- char '\'' >> parseExpr
    return $ List [Atom "quote", x]

parseList :: Parser LispVal
parseList = optional spaces >> List <$> many parseExpr

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- optional spaces >> many parseExpr
    tail <- char '.' >> spaces >> parseExpr
    optional spaces
    return $ DottedList head tail

parseComment :: Parser ()
parseComment = many1 (char ';') >> skipMany (noneOf "\n")

parseExpr :: Parser LispVal
parseExpr = do
    skipMany parseComment
    x <- (parseChar
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseAtom
         <|> between (char '(') (char ')') (try parseDottedList <|> parseList))
    skipMany (spaces <|> parseComment)
    return x

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
                             Left  err -> throwError $ Parser err
                             Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (many parseExpr)


showVal :: LispVal -> String
showVal (String contents)      = show contents
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Character c)          = "#\\" ++ [c]
showVal (PrimitiveFunc _)      = "<primitive>"
showVal (Port _)               = "<IO port>"
showVal (IOFunc _)             = "<IO primitive>"
showVal Func {params = args, vararg = varargs, body = body, closure = env} =
              "(lambda (" ++ unwords (map show args)
                          ++ (case varargs of
                                   Nothing  -> ""
                                   Just arg -> " . "  ++ arg) ++ ") "
                          ++ (unwords (showVal <$> body)) ++ ")"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _)    = return val
eval env val@(Number _)    = return val
eval env val@(Bool _)      = return val
eval env val@(Character _) = return val

-- bound variable lookup:
eval env (Atom id) = getVar env id

eval env (List [Atom "quote", val]) = return val

-- loosely-typed "if":
eval env (List [Atom "if", pred, conseq, alt]) =
    do result <- eval env pred
       case result of Bool False -> eval env alt
                      _ -> eval env conseq
------ version of "if" that only allows Boolean predicates:
-- eval env (List [Atom "if", Bool pred, conseq, alt]) =
--     do result <- eval $ Bool pred
--        case result of Bool False -> eval alt
--                       _ -> eval conseq
-- eval env (List [Atom "if", pred, _, _]) = throwError $ TypeMismatch "bool" pred

eval env (List [Atom "set!",   Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var

eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var

eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body

eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body

eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body

eval env (List [Atom "load", String filename]) =
    load filename >>= fmap last . mapM (eval env)

-- Function application:
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal

apply (PrimitiveFunc func) args = liftThrows $ func args

apply (Func params varargs body closure) args =
      if num params /= num args && isNothing varargs
         then throwError $ NumArgs (num params) args
         else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = fmap last $ mapM (eval env) body
            bindVarArgs arg env =
              case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env

apply (IOFunc func) args = func args

apply badArg _ = throwError $ TypeMismatch "function" $ badArg


ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("apply", applyProc)
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read", readProc)
               , ("write", writeProc)
               , ("read-contents", readContents)
               , ("read-all", readAll)
               ]


primitiveBindings :: IO Env
primitiveBindings =
    nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                               ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)


applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = fmap List $ load filename

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", primSubtract)
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("symbol?", isSymbol)
             , ("string?", isString)
             , ("number?", isNumber)
             , ("char?", isChar)
             , ("=", numBoolBinop (==))
             , ("<", numBoolBinop (<))
             , (">", numBoolBinop (>))
             , ("/=", numBoolBinop (/=))
             , (">=", numBoolBinop (>=))
             , ("<=", numBoolBinop (<=))
             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))
             , ("string=?", strBoolBinop (==))
             , ("string<?", strBoolBinop (<))
             , ("string>?", strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("eqv?", eqv)
             , ("eq?", eqv)
             , ("equal?", equal)
             ]

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol _ = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString _          = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber _          = return $ Bool False

isChar :: [LispVal] -> ThrowsError LispVal
isChar [Character _] = return $ Bool True
isChar _             = return $ Bool False

primSubtract :: [LispVal] -> ThrowsError LispVal
primSubtract []    = throwError $ NumArgs 2 []
primSubtract [val] = unpackNum val >>= return . Number . (0 -)
primSubtract vals  = mapM unpackNum vals >>= return . Number . foldl1 (-)

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) =  return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]             = return $ List [x1]
cons [x,  List xs]             = return $ List (x : xs)
cons [x,  DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                  = return $ DottedList [x1] x2
cons badArgList                = throwError $ NumArgs 2 badArgList


eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]

eqv [(List arg1), (List arg2)] =
    return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left  err -> False
                               Right (Bool val) -> val

eqv [_, _]     = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)


unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do unpacked1 <- unpacker arg1
       unpacked2 <- unpacker arg2
       return $ unpacked1 == unpacked2
    `catchError` (const $ return False)


equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] =
    do primitiveEquals <- fmap or $ mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
       eqvEquals <- eqv [arg1, arg2]
       return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)

equal badArgList = throwError $ NumArgs 2 badArgList


--------------------- REPL

evalString :: Env -> String -> IO String
evalString env expr =
    runIOThrows $ fmap show $ (liftThrows $ readExpr expr) >>= eval env

evalFile env filename = eval env (List [Atom "load", String filename])

runFile :: [String] -> IO ()
runFile args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ fmap show $ evalFile env filename) >>= hPutStrLn stderr
    where filename = args !! 0

runRepl :: IO ()
runRepl = primitiveBindings >>= runInputT haskelineSettings . withInterrupt . loop 0
    where haskelineSettings :: Settings IO
          haskelineSettings = defaultSettings {historyFile = Nothing}

          loop :: Int -> Env -> InputT IO ()
          loop n env = do
            minput <- handle (\Interrupt -> outputStrLn "Interrupted" >> return Nothing)
                            (getInputLine $ "["++show n++"] Lisp >>> ")
            case minput of
              Nothing -> return ()
              Just i ->
                case strip i of
                  "quit" -> return ()
                  "(quit)" -> return ()
                  "" -> loop (n+1) env
                  input -> liftIO (evalString env input) >>= outputStrLn >> loop (n+1) env


---------------------- STATE

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef[]

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . isJust . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value =
    do alreadyDefined <- liftIO $ isBound envRef var
       if alreadyDefined
          then setVar envRef var value >> return value
          else liftIO $ do
               valueRef <- newIORef value
               env <- readIORef envRef
               writeIORef envRef ((var, valueRef) : env)
               return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

main :: IO ()
main = do args <- getArgs
          if null args
             then runRepl >> putStrLn "bye!"
             else runFile args
