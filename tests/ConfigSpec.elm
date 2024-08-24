module ConfigSpec exposing (..)

import Expect
import Forester.Config exposing (Config, config)
import Json.Decode exposing (decodeString)
import Test exposing (Test)


example : String
example =
    """
{"trees":["trees"],"assets":["assets"],"theme":"theme","root":"hello","stylesheet":"default.xsl"}
"""


expected : Config
expected =
    { trees = [ "trees" ]
    , assets = [ "assets" ]
    , theme = "theme"
    , root = "hello"
    , stylesheet = "default.xsl"
    }


suite : Test
suite =
    Test.describe "Config"
        [ Test.test "example" <|
            \_ -> Expect.equal (decodeString config example) (Ok expected)
        ]
