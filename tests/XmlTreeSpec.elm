module XmlTreeSpec exposing (suite)

import Base exposing (Addr(..), addr)
import Expect
import Json.Decode exposing (decodeString)
import Test exposing (Test)
import Xml_tree exposing (Article, Content(..), Frontmatter, article, content, frontmatter)


frontmatter_str : String
frontmatter_str =
    """
{ 
    "addr": {
      "User_addr": "queries"
    },
    "source_path": "/home/kento/ocaml-forester/trees/queries.tree"
}
"""


expected_frontmatter : Frontmatter Content
expected_frontmatter =
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


str : String
str =
    """
{
  "frontmatter": {
    "addr": {
      "User_addr": "queries"
    },
    "source_path": "/home/kento/ocaml-forester/trees/queries.tree"
  },
  "mainmatter": [
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
  ],
  "backmatter": [
    {
      "Section": {
        "frontmatter": {
          "addr": "Anon",
          "title": [
            {
              "Text": "references"
            }
          ]
        },
        "mainmatter": [
          {
            "Results_of_query": {
              "Isect": [
                {
                  "Union_fam": [
                    {
                      "Rel": [
                        "Paths",
                        "Outgoing",
                        "org.forester.rel.transclusion",
                        {
                          "Addr": {
                            "User_addr": "queries"
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
                },
                {
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
              ]
            }
          }
        ],
        "flags": {
          "hidden_when_empty": true,
          "metadata_shown": false
        }
      }
    },
    {
      "Section": {
        "frontmatter": {
          "addr": "Anon",
          "title": [
            {
              "Text": "context"
            }
          ]
        },
        "mainmatter": [
          {
            "Results_of_query": {
              "Rel": [
                "Edges",
                "Incoming",
                "org.forester.rel.transclusion",
                {
                  "Addr": {
                    "User_addr": "queries"
                  }
                }
              ]
            }
          }
        ],
        "flags": {
          "hidden_when_empty": true,
          "metadata_shown": false
        }
      }
    },
    {
      "Section": {
        "frontmatter": {
          "addr": "Anon",
          "title": [
            {
              "Text": "backlinks"
            }
          ]
        },
        "mainmatter": [
          {
            "Results_of_query": {
              "Rel": [
                "Edges",
                "Incoming",
                "org.forester.rel.links",
                {
                  "Addr": {
                    "User_addr": "queries"
                  }
                }
              ]
            }
          }
        ],
        "flags": {
          "hidden_when_empty": true,
          "metadata_shown": false
        }
      }
    },
    {
      "Section": {
        "frontmatter": {
          "addr": "Anon",
          "title": [
            {
              "Text": "related"
            }
          ]
        },
        "mainmatter": [
          {
            "Results_of_query": {
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
          }
        ],
        "flags": {
          "hidden_when_empty": true,
          "metadata_shown": false
        }
      }
    },
    {
      "Section": {
        "frontmatter": {
          "addr": "Anon",
          "title": [
            {
              "Text": "contributions"
            }
          ]
        },
        "mainmatter": [
          {
            "Results_of_query": {
              "Union": [
                {
                  "Rel": [
                    "Edges",
                    "Incoming",
                    "org.forester.rel.authors",
                    {
                      "Addr": {
                        "User_addr": "queries"
                      }
                    }
                  ]
                },
                {
                  "Rel": [
                    "Edges",
                    "Incoming",
                    "org.forester.rel.contributors",
                    {
                      "Addr": {
                        "User_addr": "queries"
                      }
                    }
                  ]
                }
              ]
            }
          }
        ],
        "flags": {
          "hidden_when_empty": true,
          "metadata_shown": false
        }
      }
    }
  ]
}
"""


expected : Article Content
expected =
    { frontmatter =
        { addr = User_addr ""
        , title = Content []
        , dates = []
        , attributions = []
        , taxon = Nothing
        , number = Nothing
        , designated_parent = Nothing
        , source_path = Nothing
        , tags = []
        , metas = []
        }
    , mainmatter = Content []
    , backmatter = Content []
    }


suite : Test
suite =
    Test.describe "Full"
        [ Test.describe "is able to decode articles"
            [ Test.test "frontmatter" <|
                \_ ->
                    Expect.equal
                        (decodeString frontmatter frontmatter_str)
                        (Ok expected_frontmatter)

            -- , Test.test "example" <|
            --     \_ ->
            --         Expect.equal
            --             (decodeString article str)
            --             (Ok
            --                 expected
            --             )
            ]
        ]
