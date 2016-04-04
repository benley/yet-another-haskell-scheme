{-# LANGUAGE ExistentialQuantification #-}

module Yahs.LispVal where

import Control.Monad.Except
import Data.IORef
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (NumArgs expected found)      =
            "Expected " ++ show expected
            ++ (if expected==1 then " arg;" else " args;")
            ++ " found values " ++ unwordsList found
showError (TypeMismatch expected found) =
            "Invalid type: expected " ++ expected
            ++ ", found " ++ show found
showError (Default message) = message

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

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
                             , vararg  :: Maybe String
                             , body    :: [LispVal]
                             , closure :: Env
                             }

instance Show LispVal where show = showVal

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO

type Env = IORef [(String, IORef LispVal)]

showVal :: LispVal -> String
showVal (String contents)      = show contents
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Character c)          = "#\\" ++ case c of
                                            ' '  -> "space"
                                            '\n' -> "newline"
                                            x    -> [c]
showVal (PrimitiveFunc _)      = "<primitive>"
showVal (Port _)               = "<IO port>"
showVal (IOFunc _)             = "<IO primitive>"
showVal Func {params = args, vararg = varargs, body = body, closure = env} =
              "(lambda (" ++ unwords (map show args)
                          ++ (case varargs of
                                   Nothing  -> ""
                                   Just arg -> " . "  ++ arg)
                          ++ ") " ++ unwords (showVal <$> body)
                          ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
