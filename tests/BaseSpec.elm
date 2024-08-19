module BaseSpec exposing (suite)

import Expect
import Forester.Base
    exposing
        ( Addr(..)
        , addr
        )
import Json.Decode exposing (decodeString)
import Test exposing (Test)


userAddr : String
userAddr =
    """
    { "User_addr": "asdf" }
    """


machineAddr : String
machineAddr =
    """
    { "Machine_addr": 1000 }
    """


suite : Test
suite =
    Test.describe "Base"
        [ Test.describe "is able to decode base types"
            [ Test.test "user addrs" <|
                \_ -> Expect.equal (decodeString addr userAddr) (Ok (UserAddr "asdf"))
            , Test.test "machine addrs" <|
                \_ -> Expect.equal (decodeString addr machineAddr) (Ok (MachineAddr 1000))
            ]
        ]
