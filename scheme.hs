import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Double
  | Rational Rational
  | Complex (Complex Double)
  | String String
  | Bool Bool
  | Character Char

--特殊文字用パーサ
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

--空白除去用パーサ
spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          _    -> Atom atom

parsePrefix :: Parser LispVal
parsePrefix = do char '#'
                 prefix <- letter <|> symbol
                 expression <- many(letter <|> digit)
                 return $ case prefix of
                  'x' -> Number $ fst $ (readHex expression) !! 0
                  'd' -> (Number . read) expression
                  'o' -> Number $ fst $ (readOct expression) !! 0
                  --'b' -> readBin
                  't' -> Bool True
                  'f' -> Bool False

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space") <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
  return $ Character $ case value of
     "space" -> ' '
     "newline" -> '\n'
     otherwise -> (value !! 0)

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (parseEscape <|> noneOf "\"")
                 char '"'
                 return $ String x

parseEscape :: Parser Char
parseEscape = do char '\\'
                 x <- oneOf "\"\\nrts"
                 return $ case x of
                  'n' -> '\n'
                  'r' -> '\r'
                  't' -> '\t'
                  _   -> x

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read

parseFloat :: Parser LispVal
parseFloat = do
  head <- many1 digit
  char '.'
  tail <- many1 digit
  return $ Float $ fst $ (readFloat $ head ++ "." ++ tail) !! 0

parseRational :: Parser LispVal
parseRational = do
  denom <- many1 digit
  char '/'
  num <- many1 digit
  return $ Rational $ read(denom) % read(num)

toDouble :: LispVal -> Double
toDouble(Float f) = f
toDouble(Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do
  re <- (try parseFloat <|> do { x <- many1 digit;return $ Float $ fst $ head $ readFloat(x)})
  mark <- char '+' <|> char '-'
  im <- (try parseFloat <|> do { x <- many1 digit;return $ Float $ fst $ head $ readFloat(x)})
  char 'i'
  return $ Complex $case mark of
    '+' -> toDouble(re) :+ toDouble(im)
    '-' -> toDouble(re) :+ ( - toDouble(im))

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
   head <- endBy parseExpr spaces
   tail <- char '.' >> spaces >> parseExpr
   return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseBackQuoted :: Parser LispVal
parseBackQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseComma :: Parser LispVal
parseComma = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

--パース&表示部

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Character c) = [c]
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Rational contents) = show contents
showVal (Complex contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

--instanceとかの宣言はこういうとこでよいのか？
instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> try parsePrefix
        <|> parseCharacter
        <|> parseString
        <|> try parseRational
        <|> try parseComplex
        <|> try parseFloat
        <|> parseNumber
        <|> parseQuoted
        <|> parseBackQuoted
        <|> parseComma
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++  show val

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
