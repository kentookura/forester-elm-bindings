module Forester.Base exposing
    ( Addr(..)
    , MathMode(..)
    , XmlQname
    , addr
    , mathMode
    , xmlQname
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
    = UserAddr String
    | MachineAddr Int
    | HashAddr String
    | Anon


addr : Decoder Addr
addr =
    oneOf
        [ field "User_addr" string |> map UserAddr
        , field "Machine_addr" int |> map MachineAddr
        , field "Hash_addr" string |> map HashAddr
        , string
            |> andThen
                (\str ->
                    if str == "Anon" then
                        succeed Anon

                    else
                        fail "Failed to decode address"
                )
        ]


type MathMode
    = Inline
    | Display


mathMode : Decoder MathMode
mathMode =
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


type alias XmlQname =
    { prefix : String, uname : String, xmlns : Maybe String }


xmlQname : Decoder XmlQname
xmlQname =
    succeed XmlQname
        |> optional "prefix" string ""
        |> optional "uname" string ""
        |> optional "xmlns" (maybe string) Nothing
