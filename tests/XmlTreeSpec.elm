module XmlTreeSpec exposing (suite)

import Base exposing (Addr(..), MathMode(..), XmlQname, addr)
import Expect exposing (onFail)
import Json.Decode exposing (Decoder, decodeString, errorToString, field)
import Test exposing (Test)
import XmlTree
    exposing
        ( Article
        , Content(..)
        , ContentNode(..)
        , ContentTarget(..)
        , Frontmatter
        , Modifier(..)
        , Prim(..)
        , TeXCs_(..)
        , Transclusion
        , XmlAttr
        , XmlElt_
        , article
        , content
        , contentNode
        , contentTarget
        , defaultSectionFlags
        , emptyFrontmatterOverrides
        , frontmatter
        )


type alias Testcase a =
    ( String, a, Decoder a )


run : String -> Testcase a -> Test
run desc ( str, expected, decoder ) =
    Test.test desc <|
        \_ ->
            let
                decoded =
                    decodeString decoder str

                onFailure =
                    case decoded of
                        Ok _ ->
                            \x -> x

                        Err err ->
                            onFail (errorToString err)
            in
            Expect.equal
                decoded
                (Ok expected)
                |> onFailure


frontmatterCase : Testcase (Frontmatter Content)
frontmatterCase =
    let
        str =
            """
        { 
            "addr": {
              "User_addr": "queries"
            },
            "source_path": "/home/kento/ocaml-forester/trees/queries.tree"
        }
        """

        expected =
            { addr = UserAddr "queries"
            , attributions = []
            , dates = []
            , designated_parent = Nothing
            , metas = []
            , number = Nothing
            , source_path = Just "/home/kento/ocaml-forester/trees/queries.tree"
            , tags = []
            , taxon = Nothing
            , title = Content []
            }
    in
    ( str, expected, frontmatter )


textCase : Testcase ContentNode
textCase =
    let
        str =
            """ {"Text": "hello"}
            """

        expected =
            Text "hello"
    in
    ( str, expected, contentNode )


xmleltCase1 : Testcase ContentNode
xmleltCase1 =
    let
        str =
            """
      {
        "Xml_elt": {
          "name": {
            "prefix": "mml",
            "uname": "math",
            "xmlns": "http://www.w3.org/1998/Math/MathML"
          },
          "attrs": [
              {
                  "key": {
                      "prefix": "",
                      "uname": "style"
                  },
                  "value": "white-space:nowrap"
              }
          ],
          "content": [
            {
              "Text": "x"
            }
          ]
        }
      }
    """

        elt : XmlElt_ Content
        elt =
            { name =
                { prefix = "mml"
                , uname = "math"
                , xmlns = Just "http://www.w3.org/1998/Math/MathML"
                }
            , attrs =
                [ { key =
                        { prefix = ""
                        , uname = "style"
                        , xmlns = Nothing
                        }
                  , value = "white-space:nowrap"
                  }
                ]
            , content = Content [ Text "x" ]
            }

        expected =
            XmlElt elt
    in
    ( str, expected, contentNode )


xmleltCase2 : Testcase Content
xmleltCase2 =
    let
        str =
            """
[
    {
        "Text": " "
    },
    {
        "Text": " "
    },
    {
        "Xml_elt": {
            "name": {
                "prefix": "html",
                "uname": "tr",
                "xmlns": "http://www.w3.org/1999/xhtml"
            },
            "content": [
                {
                    "Text": " "
                },
                {
                    "Text": "  "
                },
                {
                    "Xml_elt": {
                        "name": {
                            "prefix": "html",
                            "uname": "td",
                            "xmlns": "http://www.w3.org/1999/xhtml"
                        },
                        "attrs": [
                            {
                                "key": {
                                    "prefix": "",
                                    "uname": "style"
                                },
                                "value": "white-space:nowrap"
                            }
                        ],
                        "content": [
                            {
                                "Text": " "
                            },
                            {
                                "Text": "   "
                            },
                            {
                                "Prim": [
                                    "Code",
                                    [
                                        {
                                            "Text": "x"
                                        }
                                    ]
                                ]
                            },
                            {
                                "Text": " "
                            },
                            {
                                "Text": "  "
                            }
                        ]
                    }
                },
                {
                    "Text": " "
                },
                {
                    "Text": "  "
                },
                {
                    "Xml_elt": {
                        "name": {
                            "prefix": "html",
                            "uname": "td",
                            "xmlns": "http://www.w3.org/1999/xhtml"
                        },
                        "content": [
                            {
                                "Text": "y"
                            }
                        ]
                    }
                },
                {
                    "Text": " "
                },
                {
                    "Text": " "
                }
            ]
        }
    },
    {
        "Text": " "
    }
]
"""

        expected =
            Content
                [ Text " "
                , Text " "
                , XmlElt { attrs = [], content = Content [ Text " ", Text "  ", XmlElt { attrs = [ { key = { prefix = "", uname = "style", xmlns = Nothing }, value = "white-space:nowrap" } ], content = Content [ Text " ", Text "   ", Prim ( Code, Content [ Text "x" ] ), Text " ", Text "  " ], name = { prefix = "html", uname = "td", xmlns = Just "http://www.w3.org/1999/xhtml" } }, Text " ", Text "  ", XmlElt { attrs = [], content = Content [ Text "y" ], name = { prefix = "html", uname = "td", xmlns = Just "http://www.w3.org/1999/xhtml" } }, Text " ", Text " " ], name = { prefix = "html", uname = "tr", xmlns = Just "http://www.w3.org/1999/xhtml" } }
                , Text " "
                ]
    in
    ( str, expected, content )


katexCase : Testcase ContentNode
katexCase =
    let
        str =
            """
          {
            "KaTeX": [
              "Display",
              [
                {
                  "Text": "a=b"
                }
              ]
            ]
          }
"""

        expected =
            KaTeX Display (Content [ Text "a=b" ])
    in
    ( str, expected, contentNode )


texCsCase : Testcase ContentNode
texCsCase =
    let
        str =
            """
  {
    "TeX_cs": {
      "Word": "img"
    }
  }
"""

        expected =
            TeXCs (Word "img")
    in
    ( str, expected, contentNode )


targetCase : Testcase (ContentTarget Content)
targetCase =
    let
        str =
            """
        { "Full": [
                {
                  "metadata_shown": false
                },
                {}
              ]
            }
    """

        expected =
            Full defaultSectionFlags emptyFrontmatterOverrides
    in
    ( str, expected, contentTarget content )


transcludeCase : Testcase ContentNode
transcludeCase =
    let
        str =
            """
    {
      "Transclude": {
        "addr": {
          "Machine_addr": 31999
        },
        "target": {
          "Full": [
            {
              "metadata_shown": false
            },
            {}
          ]
        },
        "modifier": "Identity"
      }
    }
    """

        expected =
            Transclude
                { addr = MachineAddr 31999
                , target = Full defaultSectionFlags emptyFrontmatterOverrides
                , modifier = Identity
                }
    in
    ( str, expected, contentNode )


suite : Test
suite =
    Test.describe "Xml_tree"
        [ Test.describe "is able to decode content nodes"
            [ run "text" textCase
            , run "Xml_elt1" xmleltCase1
            , run "Xml_elt2" xmleltCase2
            , run "Transclude" transcludeCase
            , run "KaTeX" katexCase
            , run "TeX_cs" texCsCase

            -- , run "CDATA" cdataCase
            -- , run "Results_of_query" resultsCase
            -- , run "Section" sectionCase
            -- , run "Prim" primCase
            -- , run "Link" linkCase
            -- , run "Img" imgCase
            -- , run "Resource" resourceCase
            ]
        , Test.describe "is able to decode articles"
            [ run "frontmatter" frontmatterCase
            , run "target" targetCase
            , run "transclude" transcludeCase
            ]
        ]
