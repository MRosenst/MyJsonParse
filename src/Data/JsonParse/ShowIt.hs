module Data.JsonParse.ShowIt
  ( prettyShow
  ) where

import Data.Char
import Data.JsonParse.JsonValue
import Text.Printf

indentation :: Int
indentation = 2


stripLast :: String -> String
stripLast = (++"\n") . init . init

normalizeChar :: Char -> String
normalizeChar c = 
  case c of
    '\"' -> "\\\""
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    x -> if isAscii x && not (isControl x) then [x] else '\\' : printf "u%04X" (ord x)

prettyShow :: JsonValue -> String
prettyShow = prettyShow' 0

prettyShow' :: Int -> JsonValue -> String
prettyShow' n val = 
  case val of
    NullVal  -> "null"
    Bool b   -> if b then "true" else "false"
    Number x -> show x
    String s -> "\"" ++ concatMap normalizeChar s ++ "\""
    Array a  -> "[\n" ++ stripLast (concatMap ((++",\n") . (indent++) . prettyShow' (n+1)) a) ++ "]"
    Object o -> "{\n" ++ stripLast (concat 
                [indent ++ show key ++ ": " ++ prettyShow' (n+1) val ++ ",\n" | (key, val)<-o]
                ) ++ indent ++ "}"
    where
      indent = replicate (indentation * n) ' '