module Yahs.Scheme where

import Control.Monad
import Control.Monad.Except
import Data.IORef
import Data.Maybe (isJust, isNothing)
import System.Console.Haskeline
import System.IO

import Yahs.LispVal
import Yahs.Parse
import Yahs.Primitives
import Yahs.Util

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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

-- '(quoted lists)
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

eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var

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

eval env badForm =
    throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal

apply (PrimitiveFunc func) args = liftThrows $ func args

apply (Func params varargs body closure) args =
      if num params /= num args && isNothing varargs
         then throwError $ NumArgs (num params) args
         else liftIO (bindVars closure $ zip params args)
                >>= bindVarArgs varargs
                >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = fmap last $ mapM (eval env) body
            bindVarArgs arg env =
              case arg of
                Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                Nothing -> return env

apply (IOFunc func) args = func args

apply badArg _ = throwError $ TypeMismatch "function" badArg


ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("apply", applyProc)
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read", readProc)
               , ("write", writeProc)
               , ("display", displayProc)
               , ("read-contents", readContents)
               , ("read-all", readAll)
               ]


primitiveBindings :: IO Env
primitiveBindings =
    nullEnv >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives
                               ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)


applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.6.1
closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO (hClose port) >> return (Bool True)
closePort _           = return (Bool False)

-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.6.2
readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc [badArg]    = throwError $ TypeMismatch "port" badArg
readProc badargs     =
    throwError $ Default ("Expected 0 or 1 args; found values "
                          ++ unwordsList badargs)

-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.6.3
writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc []               = throwError $ Default ("Expected 1 or 2 args; got 0")
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO (hPrint port obj) >> return (Bool True)
writeProc [_, badarg]      = throwError $ TypeMismatch "port" badarg
writeProc badargs          = throwError $ Default ("Expected 1 or 2 args; found values "
                                                   ++ unwordsList badargs)

-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.6.3
displayProc :: [LispVal] -> IOThrowsError LispVal
displayProc []               = throwError $ NumArgs 2 []
displayProc [obj]            = displayProc [obj, Port stdout]
displayProc [obj, Port port] =
    liftIO (hPutStr port (toStr obj)) >> return (Bool True)
    where
      toStr :: LispVal -> String
      toStr (String x)    = x
      toStr (Character x) = [x]
      toStr x             = show x
displayProc tooManyArgs      = throwError $ NumArgs 2 tooManyArgs


readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename

--------------------- REPL

evalString :: Env -> String -> IO String
evalString env expr =
    runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalFile env filename = eval env (List [Atom "load", String filename])

runFile :: [String] -> IO ()
runFile args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows (show <$> evalFile env filename) >>= hPutStrLn stderr
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
              Just i  ->
                case strip i of
                  "quit"   -> return ()
                  "(quit)" -> return ()
                  ""       -> loop (n+1) env
                  input    -> liftIO (evalString env input) >>= outputStrLn >> loop (n+1) env


---------------------- STATE

nullEnv :: IO Env
nullEnv = newIORef[]


liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err
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
                                   (liftIO . flip writeIORef value)
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
bindVars envRef bindings =
    readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)
