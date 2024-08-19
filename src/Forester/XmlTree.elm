module Forester.XmlTree exposing
    ( Article
    , Attribution(..)
    , Content(..)
    , ContentNode(..)
    , ContentTarget(..)
    , Frontmatter
    , FrontmatterOverrides
    , Img(..)
    , InlineImg
    , Link_
    , MetaPart(..)
    , Modifier(..)
    , Prim(..)
    , ResourceSource
    , Resource_
    , SectionFlags
    , Section_
    , TeXCs_(..)
    , Transclusion
    , XmlAttr
    , XmlElt_
    , article
    , attribution
    , content
    , contentNode
    , contentTarget
    , createPairs
    , defaultSectionFlags
    , emptyFrontmatterOverrides
    , folder
    , frontmatter
    , frontmatterOverrides
    , img
    , inlineImg
    , link
    , metas
    , modifier
    , part
    , prim
    , resource
    , resource_source
    , section
    , sectionFlags
    , texCs
    , transclusion
    , xmlAttr
    , xmlElt
    )

import Forester.Base exposing (Addr(..), MathMode, XmlQname, addr, mathMode, xmlQname)
import Forester.Prelude exposing (Date, date)
import Forester.Query as Query
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


type alias SectionFlags =
    { hidden_when_empty : Maybe Bool
    , included_in_toc : Maybe Bool
    , header_shown : Maybe Bool
    , metadata_shown : Maybe Bool
    , numbered : Maybe Bool
    , expanded : Maybe Bool
    }


defaultSectionFlags : SectionFlags
defaultSectionFlags =
    { hidden_when_empty = Nothing
    , included_in_toc = Nothing
    , header_shown = Nothing
    , metadata_shown = Just False
    , numbered = Nothing
    , expanded = Nothing
    }


sectionFlags : Decoder SectionFlags
sectionFlags =
    succeed SectionFlags
        |> optional "hidden_when_empty" (maybe bool) Nothing
        |> optional "included_in_toc" (maybe bool) Nothing
        |> optional "header_shown" (maybe bool) Nothing
        |> optional "metadata_shown" (maybe bool) Nothing
        |> optional "numbered" (maybe bool) Nothing
        |> optional "expanded" (maybe bool) Nothing


type alias FrontmatterOverrides content =
    { title : Maybe content
    , taxon : Maybe (Maybe String)
    }


emptyFrontmatterOverrides : FrontmatterOverrides content
emptyFrontmatterOverrides =
    { title = Nothing, taxon = Nothing }


frontmatterOverrides : Decoder content -> Decoder (FrontmatterOverrides content)
frontmatterOverrides c =
    succeed FrontmatterOverrides
        |> optional "title" (maybe c) Nothing
        |> optional "taxon" (maybe (maybe string)) Nothing


type alias XmlAttr =
    { key : XmlQname, value : String }


xmlAttr : Decoder XmlAttr
xmlAttr =
    succeed XmlAttr
        |> optional "key" xmlQname { prefix = "", uname = "", xmlns = Nothing }
        |> optional "value" string ""


type alias XmlElt_ content =
    { name : XmlQname, attrs : List XmlAttr, content : content }


xmlElt : Decoder (XmlElt_ Content)
xmlElt =
    succeed XmlElt_
        |> required "name" xmlQname
        |> optional "attrs" (list xmlAttr) []
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


emptyFrontmatter : Frontmatter Content
emptyFrontmatter =
    { addr = Anon
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
    { frontmatter : Frontmatter content, mainmatter : content, flags : SectionFlags }


section : Decoder (Section_ Content)
section =
    succeed
        Section_
        |> optional "frontmatter" frontmatter emptyFrontmatter
        |> optional "mainmatter" content (Content [])
        |> optional "flags" sectionFlags defaultSectionFlags


type alias Article content =
    { frontmatter : Frontmatter content
    , mainmatter : content
    , backmatter : content
    }


article : Decoder (Article Content)
article =
    succeed Article
        |> optional "frontmatter" frontmatter emptyFrontmatter
        |> optional "mainmatter" content (Content [])
        |> optional "backmatter" content (Content [])


type ContentTarget content
    = Full SectionFlags (FrontmatterOverrides content)
    | Mainmatter
    | Title
    | Taxon
    | Number


contentTarget : Decoder content -> Decoder (ContentTarget content)
contentTarget c =
    oneOf
        [ field "Full"
            (succeed
                Full
                |> optional "Section_flags" sectionFlags defaultSectionFlags
                |> optional
                    "Frontmatter_overrides"
                    (frontmatterOverrides c)
                    emptyFrontmatterOverrides
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
    = SentenceCase
    | Identity


modifier : Decoder Modifier
modifier =
    string
        |> andThen
            (\str ->
                case str of
                    "Sentence_case" ->
                        succeed SentenceCase

                    "Identity" ->
                        succeed Identity

                    _ ->
                        fail ("Failed to decode modifier: got " ++ str ++ ". Should be one of Sentence case, Identity")
            )


type alias Transclusion content =
    { addr : Addr, target : ContentTarget content, modifier : Modifier }


transclusion : Decoder content -> Decoder (Transclusion content)
transclusion c =
    succeed Transclusion
        |> required "addr" addr
        |> required "target" (contentTarget c)
        |> optional "modifier" modifier Identity


type alias Link_ content =
    { href : String, content : content }


link : Decoder content -> Decoder (Link_ content)
link c =
    succeed
        Link_
        |> required "href" string
        |> required "content" c


type alias InlineImg =
    { format : String, base64 : String }


inlineImg : Decoder InlineImg
inlineImg =
    succeed InlineImg
        |> optional "format" string ""
        |> optional "base64" string ""


type alias ResourceSource =
    { type_ : String, part : String, source : String }


resource_source : Decoder ResourceSource
resource_source =
    succeed ResourceSource
        |> optional "type_" string ""
        |> optional "parts" string ""
        |> optional "source" string ""


type Img
    = Inline InlineImg
    | Remote String


img : Decoder Img
img =
    oneOf
        [ map Inline (field "Inline" inlineImg)
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


type TeXCs_
    = Word String
    | Symbol Char


texCs : Decoder TeXCs_
texCs =
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


type ContentNode
    = Text String
    | CDATA String
    | XmlElt (XmlElt_ Content)
    | Transclude (Transclusion Content)
    | ResultsOfQuery (Query.Expr Query.Dbix)
    | Section (Section_ Content)
    | Prim ( Prim, Content )
    | KaTeX MathMode Content
    | TeXCs TeXCs_
    | Link (Link_ Content)
    | Img Img
    | Resource Resource_


type KaTeXParts
    = MM MathMode
    | C Content


katexPart : Decoder KaTeXParts
katexPart =
    oneOf [ mathMode |> map MM, content |> map C ]


contentNode : Decoder ContentNode
contentNode =
    oneOf
        [ field "Text" string |> map Text
        , field "CDATA" string |> map CDATA
        , field "Xml_elt" xmlElt |> map XmlElt
        , field "Transclude" (transclusion content) |> map Transclude
        , field "Results_of_query" (Query.expr int) |> map ResultsOfQuery
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
        , field "TeX_cs" texCs |> map TeXCs
        , field "Link" (link content) |> map Link
        , field "Img" img |> map Img
        , field "Resource" resource |> map Resource
        ]


type Content
    = Content (List ContentNode)


content : Decoder Content
content =
    list (lazy (\_ -> contentNode)) |> map Content


type alias Resource_ =
    { hash : String, content : Content, sources : List ResourceSource }


resource : Decoder Resource_
resource =
    succeed Resource_
        |> optional "hash" string ""
        |> optional "content" content (Content [])
        |> optional "sources" (list resource_source) []
