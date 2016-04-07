module Yahs.Parse where

import Control.Monad.Except (throwError, void)
import Text.ParserCombinators.Parsec hiding (spaces)

import Yahs.LispVal

readChar :: Char -> ThrowsError LispVal
readChar c = case parse (namedCharacter <|> anyChar) "" [c] of
               Left  err -> throwError $ Parser err
               Right val -> return (Character val)

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (many parseExpr)

-- Read multiple expressions from a string that may start
-- with a shebang line
readExprFile :: String -> ThrowsError [LispVal]
readExprFile = readOrThrow (optional parseShebangLine >> many parseExpr)

parseShebangLine :: Parser ()
parseShebangLine = void $ try (string "#!" >> many (noneOf "\n"))

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
                             Left  err -> throwError $ Parser err
                             Right val -> return val

-- Parsec's "spaces" parser matches _zero_ or more spaces.
-- We want at least 1.
spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseExpr :: Parser LispVal
parseExpr = do
    skipMany (spaces <|> parseComment)
    x <- parseHash
         <|> parseString
         <|> decimalNumber
         <|> parseQuoted
         <|> parseAtom
         <|> between (char '(') (char ')') (try parseDottedList <|> parseList)
    skipMany (spaces <|> parseComment)
    return x

parseComment :: Parser ()
parseComment = many1 (char ';') >> skipMany (noneOf "\n")

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

parseHash :: Parser LispVal
parseHash = do
    char '#'
    x <- oneOf "\\bdfotx"
    case x of
      '\\' -> Character <$> (namedCharacter <|> anyChar)
      'b'  -> binaryNumber
      'd'  -> decimalNumber
      -- 'e'  -> exactNumber
      'f'  -> return (Bool False)
      -- 'i'  -> inexactNumber
      'o'  -> octalNumber
      't'  -> return (Bool True)
      'x'  -> hexNumber

namedCharacter :: Parser Char
namedCharacter = do
    x <- string "space" <|> string "newline"
    return $ case x of
               "space"   -> ' '
               "newline" -> '\n'

binaryNumber :: Parser LispVal
binaryNumber = do
    num <- many1 (oneOf "01")
    notFollowedBy digit
    return $ Number (bin2dec num)
    where
      bin2dec :: String -> Integer
      bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
                where c2i c = if c == '0' then 0 else 1

decimalNumber :: Parser LispVal
decimalNumber = try negativeDecimalNumber <|> ((Number . read) <$> many1 digit)

negativeDecimalNumber :: Parser LispVal
negativeDecimalNumber = char '-' >> Number . read . ('-' :) <$> many1 digit

hexNumber :: Parser LispVal
hexNumber = do
    num <- many1 hexDigit
    notFollowedBy letter
    return $ Number $ read $ "0x" ++ num

octalNumber :: Parser LispVal
octalNumber = do
    num <- many1 octDigit
    notFollowedBy digit
    return $ Number $ read $ "0o" ++ num

parseQuoted :: Parser LispVal
parseQuoted = do
    x <- char '\'' >> parseExpr
    return $ List [Atom "quote", x]

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom

parseList :: Parser LispVal
parseList = List <$> many parseExpr

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- many parseExpr
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail
