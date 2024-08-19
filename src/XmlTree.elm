module XmlTree exposing
    ( Article
    , Attribution(..)
    , Content(..)
    , Content_node(..)
    , Content_target(..)
    , Frontmatter
    , Frontmatter_overrides
    , Img(..)
    , Inline_img
    , Link_
    , MetaPart(..)
    , Modifier(..)
    , Prim(..)
    , Resource_
    , Resource_source
    , Section_
    , Section_flags
    , TeX_cs(..)
    , Transclusion
    , Xml_attr
    , Xml_elt_
    , article
    , attribution
    , content
    , content_node
    , content_target
    , createPairs
    , default_section_flags
    , empty_frontmatter_overrides
    , folder
    , frontmatter
    , frontmatter_overrides
    , img
    , inline_img
    , link
    , metas
    , modifier
    , part
    , prim
    , resource
    , resource_source
    , section
    , section_flags
    , tex_cs
    , transclusion
    , xml_attr
    , xml_elt
    )

import Base exposing (Addr, addr, math_mode, xml_qname)
import Json.Decode as Decode
    exposing
        ( Decoder
        , andThen
        , bool
        , fail
        , field
        , int
        , lazy
        , list
        , map
        , maybe
        , oneOf
        , string
        , succeed
        )
import Json.Decode.Pipeline exposing (optional, required)
import Prelude exposing (Date, date)
import Query


type alias Section_flags =
    { hidden_when_empty : Maybe Bool
    , included_in_toc : Maybe Bool
    , header_shown : Maybe Bool
    , metadata_shown : Maybe Bool
    , numbered : Maybe Bool
    , expanded : Maybe Bool
    }


default_section_flags : Section_flags
default_section_flags =
    { hidden_when_empty = Nothing
    , included_in_toc = Nothing
    , header_shown = Nothing
    , metadata_shown = Just False
    , numbered = Nothing
    , expanded = Nothing
    }


section_flags : Decoder Section_flags
section_flags =
    succeed Section_flags
        |> optional "hidden_when_empty" (maybe bool) Nothing
        |> optional "included_in_toc" (maybe bool) Nothing
        |> optional "header_shown" (maybe bool) Nothing
        |> optional "metadata_shown" (maybe bool) Nothing
        |> optional "numbered" (maybe bool) Nothing
        |> optional "expanded" (maybe bool) Nothing


type alias Frontmatter_overrides content =
    { title : Maybe content
    , taxon : Maybe (Maybe String)
    }


empty_frontmatter_overrides =
    { title = Nothing, taxon = Nothing }


frontmatter_overrides : Decoder content -> Decoder (Frontmatter_overrides content)
frontmatter_overrides c =
    succeed Frontmatter_overrides
        |> optional "title" (maybe c) Nothing
        |> optional "taxon" (maybe (maybe string)) Nothing


type alias Xml_attr =
    { key : Base.Xml_qname, value : String }


xml_attr : Decoder Xml_attr
xml_attr =
    succeed Xml_attr
        |> optional "key" xml_qname { prefix = "", uname = "", xmlns = Nothing }
        |> optional "value" string ""


type alias Xml_elt_ content =
    { name : Base.Xml_qname, attrs : List Xml_attr, content : content }


xml_elt : Decoder (Xml_elt_ Content)
xml_elt =
    succeed Xml_elt_
        |> required "name" xml_qname
        |> optional "attrs" (list xml_attr) []
        |> optional "content" content (Content [])


type Attribution
    = Author String
    | Contributor String


attribution : Decoder Attribution
attribution =
    oneOf
        [ field "Author" (map Author string)
        , field "Contributor" (map Contributor string)
        ]


type alias Frontmatter content =
    { addr : Addr
    , title : content
    , dates : List Date
    , attributions : List Attribution
    , taxon : Maybe String
    , number : Maybe String
    , designated_parent : Maybe Addr
    , source_path : Maybe String
    , tags : List String
    , metas : List ( String, content )
    }


empty_frontmatter : Frontmatter Content
empty_frontmatter =
    { addr = Base.Anon
    , source_path = Nothing
    , designated_parent = Nothing
    , dates = []
    , attributions = []
    , taxon = Nothing
    , number = Nothing
    , metas = []
    , tags = []
    , title = Content []
    }


frontmatter : Decoder (Frontmatter Content)
frontmatter =
    Decode.succeed Frontmatter
        |> required "addr" addr
        |> optional "title" content (Content [])
        |> optional "dates" (list date) []
        |> optional "attributions" (list attribution) []
        |> optional "taxon" (maybe string) Nothing
        |> optional "number" (maybe string) Nothing
        |> optional "designated_parent" (maybe addr) Nothing
        |> optional "source_path" (maybe string) Nothing
        |> optional "tags" (list string) []
        |> optional "metas" (metas content) []



{--
  (string * content) list gets serialized as a heterogeneous list by repr.
  The strategy to work around that is to parse the strings into a list of
  MetaParts and then accumulate
--}


type MetaPart content
    = MetaId String
    | MetaValue content


part : Decoder content -> Decoder (MetaPart content)
part c =
    oneOf
        [ string |> map MetaId
        , c |> map MetaValue
        ]


type alias Accumulator content =
    ( Maybe (MetaPart content), List ( String, content ) )


folder : MetaPart content -> Accumulator content -> Accumulator content
folder parts ( state, acc ) =
    case state of
        Nothing ->
            case parts of
                MetaId str ->
                    ( Just (MetaId str), acc )

                _ ->
                    ( Nothing, [] )

        Just (MetaId str) ->
            case parts of
                MetaValue c ->
                    ( Nothing, ( str, c ) :: acc )

                _ ->
                    ( Nothing, acc )

        _ ->
            ( Nothing, acc )


createPairs : List (MetaPart content) -> List ( String, content )
createPairs l =
    let
        ( _, acc ) =
            List.foldr folder ( Nothing, [] ) l
    in
    List.reverse acc


metas : Decoder content -> Decoder (List ( String, content ))
metas c =
    list (part c) |> andThen (\parts -> createPairs parts |> succeed)


type alias Section_ content =
    { frontmatter : Frontmatter content, mainmatter : content, flags : Section_flags }


section : Decoder (Section_ Content)
section =
    succeed
        Section_
        |> optional "frontmatter" frontmatter empty_frontmatter
        |> optional "mainmatter" content (Content [])
        |> optional "flags" section_flags default_section_flags


type alias Article content =
    { frontmatter : Frontmatter content
    , mainmatter : content
    , backmatter : content
    }


article : Decoder (Article Content)
article =
    succeed Article
        |> optional "frontmatter" frontmatter empty_frontmatter
        |> optional "mainmatter" content (Content [])
        |> optional "backmatter" content (Content [])


type Content_target content
    = Full Section_flags (Frontmatter_overrides content)
    | Mainmatter
    | Title
    | Taxon
    | Number


content_target : Decoder content -> Decoder (Content_target content)
content_target c =
    oneOf
        [ field "Full"
            (succeed
                Full
                |> optional "Section_flags" section_flags default_section_flags
                |> optional
                    "Frontmatter_overrides"
                    (frontmatter_overrides c)
                    empty_frontmatter_overrides
            )
        , string
            |> andThen
                (\str ->
                    case str of
                        "Mainmatter" ->
                            succeed Mainmatter

                        "Title" ->
                            succeed Title

                        "Taxon" ->
                            succeed Taxon

                        "Number" ->
                            succeed Number

                        _ ->
                            fail "Failed to decode content_target"
                )
        ]


type Modifier
    = Sentence_case
    | Identity


modifier : Decoder Modifier
modifier =
    string
        |> andThen
            (\str ->
                case str of
                    "Sentence_case" ->
                        succeed Sentence_case

                    "Identity" ->
                        succeed Identity

                    _ ->
                        fail ("Failed to decode modifier: got " ++ str ++ ". Should be one of Sentence case, Identity")
            )


type alias Transclusion content =
    { addr : Addr, target : Content_target content, modifier : Modifier }


transclusion : Decoder content -> Decoder (Transclusion content)
transclusion c =
    succeed Transclusion
        |> required "addr" addr
        |> required "target" (content_target c)
        |> optional "modifier" modifier Identity


type alias Link_ content =
    { href : String, content : content }


link : Decoder content -> Decoder (Link_ content)
link c =
    succeed
        Link_
        |> required "href" string
        |> required "content" c


type alias Inline_img =
    { format : String, base64 : String }


inline_img : Decoder Inline_img
inline_img =
    succeed Inline_img
        |> optional "format" string ""
        |> optional "base64" string ""


type alias Resource_source =
    { type_ : String, part : String, source : String }


resource_source : Decoder Resource_source
resource_source =
    succeed Resource_source
        |> optional "type_" string ""
        |> optional "parts" string ""
        |> optional "source" string ""


type Img
    = Inline Inline_img
    | Remote String


img : Decoder Img
img =
    oneOf
        [ map Inline (field "Inline" inline_img)
        , map Remote (field "Remote" string)
        ]


type Prim
    = P
    | Ol
    | Ul
    | Li
    | Em
    | Strong
    | Code
    | Blockquote
    | Pre
    | Figure
    | Figcaption


type PrimPart
    = PP Prim
    | CL Content


prim : Decoder Prim
prim =
    string
        |> andThen
            (\str ->
                case str of
                    "P" ->
                        succeed P

                    "Ol" ->
                        succeed Ol

                    "Ul" ->
                        succeed Ul

                    "Li" ->
                        succeed Li

                    "Em" ->
                        succeed Em

                    "Strong" ->
                        succeed Strong

                    "Code" ->
                        succeed Code

                    "Blockquote" ->
                        succeed Blockquote

                    "Pre" ->
                        succeed Pre

                    "Figure" ->
                        succeed Figure

                    "Figcaption" ->
                        succeed Figcaption

                    _ ->
                        fail "failed to decode Prim"
            )


primPart : Decoder PrimPart
primPart =
    oneOf
        [ prim |> map PP
        , content |> map CL
        ]


type TeX_cs
    = Word String
    | Symbol Char


tex_cs : Decoder TeX_cs
tex_cs =
    oneOf
        [ map Word
            (field "Word" string)
        , map Symbol
            (field "Symbol" string
                |> andThen
                    (\str ->
                        case String.toList str of
                            [ char ] ->
                                succeed char

                            [] ->
                                fail "Failed to decode TeX_cs. Empty char can not happen"

                            _ ->
                                fail "Failed to decode TeX_cs. Too many chars."
                    )
            )
        ]


type Content_node
    = Text String
    | CDATA String
    | Xml_elt (Xml_elt_ Content)
    | Transclude (Transclusion Content)
    | Results_of_query (Query.Expr Query.Dbix)
    | Section (Section_ Content)
    | Prim ( Prim, Content )
    | KaTeX Base.Math_mode Content
    | TeX_cs TeX_cs
    | Link (Link_ Content)
    | Img Img
    | Resource Resource_


type KaTeXParts
    = MM Base.Math_mode
    | C Content


katexPart : Decoder KaTeXParts
katexPart =
    oneOf [ math_mode |> map MM, content |> map C ]


content_node : Decoder Content_node
content_node =
    oneOf
        [ field "Text" string |> map Text
        , field "CDATA" string |> map CDATA
        , field "Xml_elt" xml_elt |> map Xml_elt
        , field "Transclude" (transclusion content) |> map Transclude
        , field "Results_of_query" (Query.expr int) |> map Results_of_query
        , field "Section" section |> map Section
        , field "Prim"
            (list primPart
                |> andThen
                    (\parts ->
                        case parts of
                            [ PP p, CL c ] ->
                                succeed (Prim ( p, c ))

                            _ ->
                                fail "failed to decode prim"
                    )
            )
        , field "KaTeX"
            (list katexPart
                |> andThen
                    (\parts ->
                        case parts of
                            [ MM m, C c ] ->
                                succeed (KaTeX m c)

                            _ ->
                                fail "failed to decode katex"
                    )
            )
        , field "TeX_cs" tex_cs |> map TeX_cs
        , field "Link" (link content) |> map Link
        , field "Img" img |> map Img
        , field "Resource" resource |> map Resource
        ]


type Content
    = Content (List Content_node)


content : Decoder Content
content =
    list (lazy (\_ -> content_node)) |> map Content


type alias Resource_ =
    { hash : String, content : Content, sources : List Resource_source }


resource : Decoder Resource_
resource =
    succeed Resource_
        |> optional "hash" string ""
        |> optional "content" content (Content [])
        |> optional "sources" (list resource_source) []
