module ParserSpec exposing (suite)

import Expect
import Forester.Base exposing (Visibility(..))
import Forester.Parser exposing (export, import_, textualNode)
import Forester.Parser.Utils exposing (txtArg)
import Forester.Syntax exposing (Node(..))
import Parser exposing (run)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Parser"
        [ Test.describe "nodes"
            [ Test.test "text node" <|
                \_ ->
                    Expect.equal
                        (run textualNode "asdf")
                        (Ok (Text "asdf"))
            , Test.test "import" <|
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



{--
cases =
    [ Text "asdf" ]
        |> List.map
            (\node ->
                case node of
                    Text _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Verbatim _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Group _ _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Math _ _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Ident _ _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    XmlTag _ _ _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Subtree _ _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Let _ _ _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Open _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Scope _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Put _ _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Default _ _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Get _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Fun _ _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Object _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Patch _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Call _ _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Import _ _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Def _ _ _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    DeclXmlns _ _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Alloc _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")

                    Namespace _ _ ->
                        Test.test "Text node" <|
                            \_ ->
                                Expect.equal (Ok node)
                                    (run Parser.node "asdf")
            )
            --}
