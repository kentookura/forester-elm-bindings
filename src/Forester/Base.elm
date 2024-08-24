module Forester.Base exposing
    ( Addr(..)
    , Binding
    , BindingStrategy(..)
    , Delim(..)
    , MathMode(..)
    , Visibility(..)
    , XmlQname
    , addr
    , delimToString
    , mathMode
    , ppAddr
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


type Delim
    = Braces
    | Squares
    | Parens


delimToString : Delim -> ( String, String )
delimToString d =
    case d of
        Braces ->
            ( "{", "}" )

        Squares ->
            ( "[", "]" )

        Parens ->
            ( "(", ")" )


type BindingStrategy
    = Lazy
    | Strict


type alias Binding a =
    ( BindingStrategy, a )


type Visibility
    = Private
    | Public


type Addr
    = UserAddr String
    | MachineAddr Int
    | HashAddr String
    | Anon


ppAddr : Addr -> String
ppAddr a =
    case a of
        UserAddr str ->
            str

        MachineAddr i ->
            "#" ++ String.fromInt i

        HashAddr h ->
            "<hash:" ++ h ++ ">"

        Anon ->
            "<anon>"


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
