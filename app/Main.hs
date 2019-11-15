module Main where

import Lib
import Text.Parsec.Text.Lazy

main = do
  res <- parseFromFile json "app\\test.json"
  case res of
    Left err  -> print err
    Right val -> print val
