module QuerySpec exposing (suite)

import Base exposing (Addr(..), addr)
import Expect
import Json.Decode exposing (decodeString)
import Query
    exposing
        ( Addr_expr(..)
        , Expr(..)
        , Mode(..)
        , Polarity(..)
        , expr
        )
import Test exposing (Test)


r : Expr addr
r =
    Rel
        Paths
        Outgoing
        "org.forester.rel.transclusion"
        (Addr (User_addr "asdf"))


r_str : String
r_str =
    """
    { "Rel": [
        "Paths",
        "Outgoing",
        "org.forester.rel.transclusion",
        {
            "Addr": {
                "User_addr": "asdf"
            }
        }
    ]
    }
"""


e : Expr Addr
e =
    Isect
        [ Union_fam
            (Rel
                Paths
                Outgoing
                "org.forester.rel.transclusion"
                (Addr (User_addr "asdf"))
            )
            { body =
                Rel
                    Edges
                    Outgoing
                    "org.forester.rel.links"
                    (Var (Machine_addr 0))
            }
        ]


e_str : String
e_str =
    """
    {"Isect" : [
        {
            "Union_fam": [
                {
                    "Rel": [
                        "Paths",
                        "Outgoing",
                        "org.forester.rel.transclusion",
                        {
                            "Addr": {
                                "User_addr": "asdf"
                            }
                        }
                    ]
                },
                {
                    "body": {
                        "Rel": [
                            "Edges",
                            "Outgoing",
                            "org.forester.rel.links",
                            {
                                "Var": 0
                            }
                        ]
                    }
                }
            ]
        },
        {
            "Rel": [
                "Edges",
                "Incoming",
                "org.forester.rel.taxa",
                {
                    "Addr": {
                        "User_addr": "reference"
                    }
                }
            ]
        }
    ]
    }
"""


suite : Test
suite =
    Test.describe "Query"
        [ Test.describe "is able to decode queries"
            [ Test.test "user addrs" <|
                \_ -> Expect.equal (decodeString (expr addr) r_str) (Ok r)

            -- Test.test "user addrs" <| \_ -> Expect.equal (decodeString (expr addr) e_str) (Ok e)
            ]
        ]
