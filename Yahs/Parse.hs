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
    x <- parseChar
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseAtom
         <|> between (char '(') (char ')') (try parseDottedList <|> parseList)
    skipMany (spaces <|> parseComment)
    return x

parseComment :: Parser ()
parseComment = many1 (char ';') >> skipMany (noneOf "\n")

parseChar :: Parser LispVal
parseChar = Character <$> try (string "#\\" >> (namedCharacter <|> anyChar))

namedCharacter :: Parser Char
namedCharacter = do
    x <- string "space" <|> string "newline"
    return $ case x of
               "space"   -> ' '
               "newline" -> '\n'

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

parseNumber :: Parser LispVal
parseNumber = decimalNumber
              <|> binaryRadixNumber
              <|> hexRadixNumber
              <|> octalRadixNumber

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
    num <- try (string "#o" >> many1 octDigit)
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
    return $ case atom of
               "#t" -> Bool True
               "#f" -> Bool False
               _    -> Atom atom

parseList :: Parser LispVal
parseList = List <$> many parseExpr

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- many parseExpr
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail
