module Data.JsonParse.ParseIt
    ( parseFile
    , parse
    ) where

import Data.Bifunctor
import Data.JsonParse.JsonValue
import Data.Text.Lazy
import qualified Text.Parsec as P
import Text.Parsec.Text.Lazy

showError :: Either P.ParseError a -> Either String a
showError = first show

parseFile :: FilePath -> IO (Either String JsonValue)
parseFile = fmap showError . parseFromFile json

parse :: Text -> Either String JsonValue
parse = showError . P.parse json ""