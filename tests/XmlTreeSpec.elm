module XmlTreeSpec exposing (suite)

import Base exposing (Addr(..), Math_mode(..), addr)
import Expect
import Json.Decode exposing (Decoder, decodeString)
import Test exposing (Test)
import XmlTree
    exposing
        ( Article
        , Content(..)
        , Content_node(..)
        , Content_target(..)
        , Frontmatter
        , Modifier(..)
        , TeX_cs(..)
        , Transclusion
        , article
        , content
        , content_node
        , content_target
        , default_section_flags
        , empty_frontmatter_overrides
        , frontmatter
        )


type alias Testcase a =
    ( String, a, Decoder a )


run : String -> Testcase a -> Test
run desc ( str, expected, decoder ) =
    Test.test desc <|
        \_ ->
            Expect.equal
                (decodeString decoder str)
                (Ok expected)


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
            { addr = User_addr "queries"
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


textCase : Testcase Content_node
textCase =
    let
        str =
            """ {"Text": "hello"}
            """

        expected =
            Text "hello"
    in
    ( str, expected, content_node )


xmleltCase : Testcase Content_node
xmleltCase =
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
          "content": [
            {
              "Text": "x"
            }
          ]
        }
      }
    """

        expected =
            Xml_elt
                { name =
                    { prefix = "mml"
                    , uname = "math"
                    , xmlns = Just "http://www.w3.org/1998/Math/MathML"
                    }
                , attrs = []
                , content = Content [ Text "x" ]
                }
    in
    ( str, expected, content_node )


katexCase : Testcase Content_node
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
    ( str, expected, content_node )


texCsCase : Testcase Content_node
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
            TeX_cs (Word "img")
    in
    ( str, expected, content_node )


targetCase : Testcase (Content_target Content)
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
            Full default_section_flags empty_frontmatter_overrides
    in
    ( str, expected, content_target content )


transcludeCase : Testcase Content_node
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
                { addr = Machine_addr 31999
                , target = Full default_section_flags empty_frontmatter_overrides
                , modifier = Identity
                }
    in
    ( str, expected, content_node )


suite : Test
suite =
    Test.describe "Xml_tree"
        [ Test.describe "is able to decode content nodes"
            [ run "text" textCase
            , run "Xml_elt" xmleltCase
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
