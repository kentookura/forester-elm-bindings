module Forester.Parser.Utils exposing
    ( braces
    , delimited
    , option
    , parens
    , squares
    , text
    , txtArg
    , wschar
    , wstext
    )

import Forester.Base exposing (Delim(..), delimToString)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , chompWhile
        , getChompedString
        , oneOf
        , succeed
        , symbol
        )
import Tuple exposing (first, second)


option : Parser a -> Parser (Maybe a)
option p =
    oneOf [ succeed Just |= p, succeed Nothing ]


braces : Parser a -> Parser a
braces p =
    delimited Braces p


squares : Parser a -> Parser a
squares p =
    delimited Squares p


parens : Parser a -> Parser a
parens p =
    delimited Parens p


delimited : Delim -> Parser a -> Parser a
delimited d p =
    let
        pair =
            delimToString d
    in
    succeed identity
        |. symbol (first pair)
        |= p
        |. symbol (second pair)


isText : Char -> Bool
isText c =
    List.all (\b -> c /= b)
        [ ' '
        , '%'
        , '#'
        , '\\'
        , '{'
        , '}'
        , '['
        , ']'
        , '('
        , ')'
        , '\u{000D}'
        , '\n'
        ]


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t'


text : Parser String
text =
    getChompedString <|
        succeed ()
            |. chompWhile
                (\c -> isText c)


txtArg : Parser String
txtArg =
    braces wstext


wschar : Parser String
wschar =
    getChompedString <|
        succeed ()
            |. chompWhile (\c -> c == ' ' || c == '\t')


wstext : Parser String
wstext =
    getChompedString <|
        succeed ()
            |. chompWhile (\c -> isText c || isWhitespace c)
