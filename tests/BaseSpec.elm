module BaseSpec exposing (suite)

import Base
    exposing
        ( Addr(..)
        , addr
        , math_mode
        , xml_qname
        )
import Expect
import Json.Decode exposing (decodeString)
import Test exposing (Test)


user_addr : String
user_addr =
    """
    { "User_addr": "asdf" }
    """


machine_addr : String
machine_addr =
    """
    { "Machine_addr": 1000 }
    """


suite : Test
suite =
    Test.describe "Base"
        [ Test.describe "is able to decode base types"
            [ Test.test "user addrs" <|
                \_ -> Expect.equal (decodeString addr user_addr) (Ok (User_addr "asdf"))
            , Test.test "machine addrs" <|
                \_ -> Expect.equal (decodeString addr machine_addr) (Ok (Machine_addr 1000))
            ]
        ]
