{-# LANGUAGE ExistentialQuantification #-}

module Yahs.Primitives where

import Control.Monad.Except (throwError, catchError)

import Yahs.LispVal

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+",         numericBinop (+))
             , ("-",         primSubtract)
             , ("*",         numericBinop (*))
             , ("/",         numericBinop div)
             , ("mod",       numericBinop mod)
             , ("quotient",  numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("symbol?",   isSymbol)
             , ("string?",   isString)
             , ("number?",   isNumber)
             , ("char?",     isChar)
             , ("=",         numBoolBinop (==))
             , ("<",         numBoolBinop (<))
             , (">",         numBoolBinop (>))
             , ("/=",        numBoolBinop (/=))
             , (">=",        numBoolBinop (>=))
             , ("<=",        numBoolBinop (<=))
             , ("&&",        boolBoolBinop (&&))
             , ("||",        boolBoolBinop (||))
             , ("string=?",  strBoolBinop (==))
             , ("string<?",  strBoolBinop (<))
             , ("string>?",  strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))
             , ("car",       car)
             , ("cdr",       cdr)
             , ("cons",      cons)
             , ("eqv?",      eqv)
             , ("eq?",       eqv)
             , ("equal?",    equal)
             ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = fmap (Number . foldl1 op) (mapM unpackNum params)

-- Allow for unary (- 2) as well as (- 1 2 3)
primSubtract :: [LispVal] -> ThrowsError LispVal
primSubtract []    = throwError $ NumArgs 2 []
primSubtract [val] = unpackNum val >>= return . Number . (0 -)
primSubtract vals  = mapM unpackNum vals >>= return . Number . foldl1 (-)

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol _        = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString _          = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber _          = return $ Bool False

isChar :: [LispVal] -> ThrowsError LispVal
isChar [Character _] = return $ Bool True
isChar _             = return $ Bool False

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left  <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right


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
eqv [(Bool arg1),       (Bool arg2)]       = return $ Bool $ arg1 == arg2
eqv [(Number arg1),     (Number arg2)]     = return $ Bool $ arg1 == arg2
eqv [(String arg1),     (String arg2)]     = return $ Bool $ arg1 == arg2
eqv [(Atom arg1),       (Atom arg2)]       = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]

eqv [(List arg1), (List arg2)] =
    return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left  err -> False
                               Right (Bool val) -> val

eqv [_, _]     = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] =
    do primitiveEquals <- fmap or (mapM (unpackEquals arg1 arg2)
                                        [AnyUnpacker unpackNum,
                                         AnyUnpacker unpackStr,
                                         AnyUnpacker unpackBool])
       eqvEquals <- eqv [arg1, arg2]
       return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)

equal badArgList = throwError $ NumArgs 2 badArgList

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) =  return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do unpacked1 <- unpacker arg1
       unpacked2 <- unpacker arg2
       return $ unpacked1 == unpacked2
    `catchError` const (return False)

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)
