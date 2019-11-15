# my-json-parse

## usage
    Prelude> import Data.JsonParse
    Prelude Data.JsonParse> parseFile "my-file.json"
    Right (Object [("this",5.0),("is a json file",True)])

    Prelude Data.JsonParse> let res = parseFile "invalid-file.json"
    Prelude Data.JsonParse> res
    Left "\"invalid-file.json\" (line 4, column 9):\nunexpected \"t\"\nexpecting \"\\\"\" or \"}\""
    Prelude Data.JsonParse> let Left msg = res
    Prelude Data.JsonParse> putStrLn msg
    "test\test.json" (line 4, column 9):
    unexpected "t"
    expecting "\"" or "}"
