module Parser.Spec exposing (suite)

import Expect
import Forester.Base exposing (BindingStrategy(..), Visibility(..))
import Forester.Parser
    exposing
        ( arg
        , binder
        , binders
        , def
        , export
        , fun
        , funSpec
        , ident
        , identWithMethodCalls
        , import_
        , let_
        , textualNode
        , textualNodes
        , xmlQname
        , xmlTag
        )
import Forester.Syntax exposing (Node(..))
import Parser exposing (run)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Parser"
        [ Test.describe "function definition"
            [ Test.test "ident" <|
                \_ ->
                    Expect.equal
                        (run ident "\\foo/bar/baz#asdf")
                        (Ok [ "foo", "bar", "baz#asdf" ])
            , Test.test "strict binder" <|
                \_ ->
                    Expect.equal
                        (run binder "[x]")
                        (Ok ( Strict, [ "x" ] ))
            , Test.test "lazy binder" <|
                \_ ->
                    Expect.equal
                        (run binder "[~x]")
                        (Ok ( Lazy, [ "x" ] ))
            , Test.test "multiple binders" <|
                \_ ->
                    Expect.equal
                        (run binders "[x][~y]")
                        (Ok [ ( Strict, [ "x" ] ), ( Lazy, [ "y" ] ) ])
            , Test.test "no binders" <|
                \_ ->
                    Expect.equal
                        (run binders "")
                        (Ok [])
            , Test.test "arg" <|
                \_ ->
                    Expect.equal
                        (run arg "{asdf \\bar}")
                        (Ok [ Text "asdf", Ident [ "bar" ] [] ])
            , Test.test "funSpec" <|
                \_ ->
                    Expect.equal
                        (run funSpec "\\foo[x]{bar}")
                        (Ok ( [ "foo" ], [ ( Strict, [ "x" ] ) ], [ Text "bar" ] ))
            , Test.test "def" <|
                \_ ->
                    Expect.equal
                        (run def "\\def\\foo[x]{bar}")
                        (Ok (Def [ "foo" ] [ ( Strict, [ "x" ] ) ] [ Text "bar" ]))
            , Test.test "let" <|
                \_ ->
                    Expect.equal
                        (run let_ "\\let\\foo[x]{bar}")
                        (Ok (Let [ "foo" ] [ ( Strict, [ "x" ] ) ] [ Text "bar" ]))
            ]
        , Test.describe "textual node"
            [ Test.test "just text" <|
                \_ ->
                    Expect.equal
                        (run textualNode "asdf")
                        (Ok (Text "asdf"))
            , Test.test "ident" <|
                \_ ->
                    Expect.equal
                        (run textualNode "\\asdf")
                        (Ok (Ident [ "asdf" ] []))
            , Test.test "empty string" <|
                \_ ->
                    Expect.err
                        (run textualNode "")
            , Test.test "mulitple nodes" <|
                \_ ->
                    Expect.equal
                        (run textualNodes "\\asdf     \\asdfasdf")
                        (Ok
                            [ Ident [ "asdf" ] []
                            , Ident [ "asdfasdf" ] []
                            ]
                        )
            ]
        , Test.describe "ident with method calls"
            [ Test.test "no methods" <|
                \_ ->
                    Expect.equal
                        (run identWithMethodCalls "\\foo/bar/baz")
                        (Ok (Ident [ "foo", "bar", "baz" ] []))
            , Test.test "methods" <|
                \_ ->
                    Expect.equal
                        (run identWithMethodCalls "\\foo/bar/baz#asdf")
                        (Ok (Ident [ "foo", "bar", "baz" ] [ "asdf" ]))
            ]
        , Test.describe "xml support"
            [ Test.test "qname" <|
                \_ ->
                    Expect.equal
                        (run xmlQname "foo:bar")
                        (Ok ( Just "foo", "bar" ))
            , Test.test "tag" <|
                \_ ->
                    Expect.equal
                        (run xmlTag "\\<foo:bar>{}")
                        (Ok (XmlTag ( Just "foo", "bar" ) [] []))
            , Test.test "with attrs" <|
                \_ ->
                    Expect.equal
                        (run xmlTag "\\<foo:bar>[a]{b}[c]{d}{}")
                        (Ok (XmlTag ( Just "foo", "bar" ) [ ( ( Nothing, "a" ), [ Text "b" ] ), ( ( Nothing, "c" ), [ Text "d" ] ) ] []))
            ]
        , Test.describe "nodes"
            [ Test.test "import" <|
                \_ ->
                    Expect.equal
                        (run import_ "\\import{asdf}")
                        (Ok (Import Private "asdf"))
            , Test.test "export" <|
                \_ ->
                    Expect.equal
                        (run export "\\export{asdf}")
                        (Ok (Import Public "asdf"))
            ]
        ]
