module Main where

    -- TODO make an actual test suite

    import Data.JsonParse
    import Text.Parsec.Text.Lazy
    
    main = do
      res <- parseFile "test\\test.json"
      case res of
        Left err  -> print err
        Right val -> print val
