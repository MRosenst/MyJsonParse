module Data.JsonParse.JsonValue 
  ( JsonValue (..)
  , json
  ) where

import Data.Char
import Text.Parsec
import Text.Parsec.Text.Lazy

data JsonValue
  = Object [(String, JsonValue)]
  | Array [JsonValue]
  | String String
  | Number Double
  | Bool Bool
  | NullVal
  deriving (Show, Eq)

controlSequence :: Parser Char
controlSequence = do
  x <- oneOf "\"\\/bfnrtu"
  case x of
    'u' -> do
      code <- count 4 hexDigit
      case readLitChar $ "\\x" ++ code of
        [(z,_)] -> return z
        _ -> unexpected $ "invalid escape code \"\\u" ++ code ++ "\""
    'b' -> return '\b'
    'f' -> return '\f'
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'
    y -> return y

str :: Parser JsonValue
str = do
  let stdChar = satisfy (\c -> not (isControl c) && c `notElem` ("\"" :: String))
  char '"'
  content <- many $ do
    c <- stdChar
    case c of
      '\\' -> controlSequence
      x -> return x

  char '"'
  return $ String content

bool :: Parser JsonValue
bool = do
  val <- string "true" <|> string "false"
  case val of
    "true" -> return $ Bool True
    "false" -> return $ Bool False

nullval :: Parser JsonValue
nullval = do
  string "null"
  return NullVal

number :: Parser JsonValue
number = do
  try $ lookAhead (char '-' <|> digit)

  minus <- optionMaybe $ char '-'
  let sign = case minus of
              Nothing ->  1
              Just _  -> -1
  
  int <- fmap read $ option "0" $ do
    lead <- oneOf "123456789"
    rest <- many digit
    return $ lead : rest
  
  fracStr <- option "0" $ do
    char '.'
    many1 digit
  
  let frac = read fracStr * 10 ** fromIntegral (- length fracStr)
  
  exp <- fmap read $ option "0" $ do
    oneOf "eE"
    expsign <- option '+' $ oneOf "+-"
    digs <- many1 digit
    return $ if expsign == '+' then digs else expsign : digs

  return $ Number (sign * (int + frac) * 10 ** fromIntegral exp)

whitespace :: Parser ()
whitespace = do
  many $ oneOf " \t\n\r"
  return ()

keyVal :: Parser (String, JsonValue)
keyVal = do
  whitespace
  String s <- str
  whitespace
  char ':'
  val <- jsonvalue
  return (s, val)

object :: Parser JsonValue
object = do
  char '{'
  whitespace
  pairs <- keyVal `sepBy` char ','
  char '}'
  return $ Object pairs

array :: Parser JsonValue
array = do
  char '['
  whitespace
  vals <- jsonvalue `sepBy` char ','
  char ']'
  return $ Array vals

jsonvalue :: Parser JsonValue
jsonvalue = do
  whitespace
  choice [object, array, str, number, bool, nullval]

json :: Parser JsonValue
json = object <|> array