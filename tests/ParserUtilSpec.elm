module ParserUtilSpec exposing (suite)

import Expect
import Forester.Parser.Utils exposing (..)
import Forester.Syntax exposing (Node(..))
import Parser exposing (getChompedString, run, symbol)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Parser utils"
        [ Test.describe "delimited"
            [ Test.test "braces" <|
                \_ ->
                    Expect.equal
                        (run (braces (getChompedString <| symbol "foo")) "{foo}")
                        (Ok "foo")
            , Test.test "squares" <|
                \_ ->
                    Expect.equal
                        (run (squares (getChompedString <| symbol "foo")) "[foo]")
                        (Ok "foo")
            , Test.test "parens" <|
                \_ ->
                    Expect.equal
                        (run (parens (getChompedString <| symbol "foo")) "(foo)")
                        (Ok "foo")
            ]
        , Test.describe "option"
            [ Test.test "Just" <|
                \_ ->
                    Expect.equal
                        (run (option (getChompedString <| symbol "foo")) "foo")
                        (Ok (Just "foo"))
            , Test.test "Nothing" <|
                \_ ->
                    Expect.equal
                        (run (option (getChompedString <| symbol "foo")) "bar")
                        (Ok Nothing)
            ]
        , Test.describe "whitespace"
            [ Test.test "wschar" <|
                \_ ->
                    Expect.equal
                        (run wschar "  \t \t")
                        (Ok "  \t \t")
            , Test.test "wsOrText" <|
                \_ ->
                    Expect.equal
                        (run wstext "foo   bar   ")
                        (Ok "foo   bar   ")
            , Test.test "symbols" <|
                \_ ->
                    Expect.equal
                        (run text "{}")
                        (Ok "")
            ]
        , Test.describe "txtArg"
            [ Test.test "regular" <|
                \_ ->
                    Expect.equal
                        (run txtArg "{asdf}")
                        (Ok "asdf")
            ]
        ]
