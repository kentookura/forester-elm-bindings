module QuerySpec exposing (suite)

import Base exposing (Addr(..), addr)
import Expect
import Json.Decode exposing (decodeString)
import Query
    exposing
        ( AddrExpr(..)
        , Dbix
        , Expr(..)
        , Mode(..)
        , Polarity(..)
        , dbix
        , expr
        )
import Test exposing (Test)


r : Expr addr
r =
    Rel
        Paths
        Outgoing
        "org.forester.rel.transclusion"
        (Addr (UserAddr "asdf"))


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


u : Expr Dbix
u =
    UnionFam
        (Rel
            Paths
            Outgoing
            "org.forester.rel.transclusion"
            (Addr (UserAddr "asdf"))
        )
        { body =
            Rel
                Edges
                Outgoing
                "org.forester.rel.links"
                (Var 0)
        }


u_str : String
u_str =
    """
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
        }
"""


e : Expr Addr
e =
    Isect
        [ Rel
            Edges
            Outgoing
            "org.forester.rel.links"
            (Addr (UserAddr "queries"))
        , Complement
            (Rel
                Edges
                Incoming
                "org.forester.rel.taxa"
                (Addr (UserAddr "reference"))
            )
        ]


e_str : String
e_str =
    """
{
              "Isect": [
                {
                  "Rel": [
                    "Edges",
                    "Outgoing",
                    "org.forester.rel.links",
                    {
                      "Addr": {
                        "User_addr": "queries"
                      }
                    }
                  ]
                },
                {
                  "Complement": {
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
                }
              ]
            }
"""


suite : Test
suite =
    Test.describe "Query"
        [ Test.describe "is able to decode queries"
            [ Test.test "union" <|
                \_ -> Expect.equal (decodeString (expr dbix) u_str) (Ok u)
            , Test.test "rel" <|
                \_ -> Expect.equal (decodeString (expr addr) r_str) (Ok r)
            , Test.test "user addrs" <|
                \_ -> Expect.equal (decodeString (expr addr) e_str) (Ok e)
            ]
        ]
