module Base exposing
    ( Addr(..)
    , Math_mode(..)
    , Xml_qname
    , addr
    , math_mode
    , xml_qname
    )

import Json.Decode
    exposing
        ( Decoder
        , andThen
        , fail
        , field
        , int
        , map
        , maybe
        , oneOf
        , string
        , succeed
        )
import Json.Decode.Pipeline exposing (optional)


type Addr
    = User_addr String
    | Machine_addr Int
    | Hash_addr String
    | Anon


addr : Decoder Addr
addr =
    oneOf
        [ field "User_addr" string |> map User_addr
        , field "Machine_addr" int |> map Machine_addr
        , field "Hash_addr" string |> map Hash_addr
        , string
            |> andThen
                (\str ->
                    if str == "Anon" then
                        succeed Anon

                    else
                        fail "Failed to decode address"
                )
        ]


type Math_mode
    = Inline
    | Display


math_mode : Decoder Math_mode
math_mode =
    string
        |> andThen
            (\str ->
                if str == "Inline" then
                    succeed Inline

                else if str == "Display" then
                    succeed Display

                else
                    fail "Failed t odecode math mode"
            )


type alias Xml_qname =
    { prefix : String, uname : String, xmlns : Maybe String }


xml_qname : Decoder Xml_qname
xml_qname =
    succeed Xml_qname
        |> optional "prefix" string ""
        |> optional "uname" string ""
        |> optional "xmlns" (maybe string) Nothing
