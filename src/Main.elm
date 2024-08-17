module Main exposing (main)

import Base exposing (Addr, addr, xml_qname)
import Browser
import Html exposing (Html, div, img)
import Json.Decode as Decode exposing (Decoder, andThen, bool, decodeString, fail, field, int, lazy, list, map, map2, map3, map6, maybe, oneOf, oneOrMore, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded)
import Maybe.Extra
import Query


type alias Date =
    { yyyy : Int, mm : Maybe Int, dd : Maybe Int }


date : Decoder Date
date =
    map3 Date
        (field "yyyy" int)
        (field "mm" (maybe int))
        (field "dd" (maybe int))


type alias Section_flags =
    { hidden_when_empty : Maybe Bool
    , included_in_toc : Maybe Bool
    , header_shown : Maybe Bool
    , metadata_shown : Maybe Bool
    , numbered : Maybe Bool
    , expanded : Maybe Bool
    }


section_flags : Decoder Section_flags
section_flags =
    map6 Section_flags
        (field "hidden_when_empty" (maybe bool))
        (field "included_in_toc" (maybe bool))
        (field "header_shown" (maybe bool))
        (field "metadata_shown" (maybe bool))
        (field "numbered" (maybe bool))
        (field "expanded" (maybe bool))


type alias Frontmatter_overrides content =
    { title : Maybe content
    , taxon : Maybe (Maybe String)
    }


frontmatter_overrides : Decoder content -> Decoder (Frontmatter_overrides content)
frontmatter_overrides c =
    map2 Frontmatter_overrides
        (field "title" (maybe c))
        (field "taxon" (maybe (maybe string)))


type alias Xml_attr =
    { key : Base.Xml_qname, value : String }


xml_attr : Decoder Xml_attr
xml_attr =
    map2 Xml_attr (field "key" xml_qname) (field "value" string)


type alias Xml_elt_ content =
    { name : Base.Xml_qname, attrs : List Xml_attr, content : content }


xml_elt : Decoder content -> Decoder (Xml_elt_ content)
xml_elt c =
    map3 Xml_elt_
        (field "name" xml_qname)
        (field "attrs" (list xml_attr))
        (field "content" c)


type Attribution
    = Author String
    | Contributor String


attribution : Decoder Attribution
attribution =
    oneOf [ field "Author" (map Author string), field "Contributor" (map Contributor string) ]


type alias Frontmatter content =
    { addr : Addr
    , title : content
    , dates : List Date
    , attributions : List Attribution
    , taxon : String
    , number : Maybe String
    , designated_parent : Maybe Addr
    , source_path : Maybe String
    , tags : List String
    , metas : List ( String, content )
    }



-- Todo: frontmatter


frontmatter : Decoder content -> Decoder (Frontmatter content)
frontmatter c =
    oneOf []


type EntryFlags
    = EntryId String
    | EntryValue (List Content_node)


type alias RecordFlags =
    List EntryFlags


type alias Record =
    ( String, List Content_node )



-- type alias Metas =
--     List MetaItem


type alias Section_ content =
    { frontmatter : Frontmatter content, mainmatter : content, flags : Section_flags }


buildRecord : List EntryFlags -> Decoder Record
buildRecord ms =
    case ms of
        [] ->
            Decode.fail "No values were passed"

        [ x ] ->
            Decode.fail "Only key was passed, but no values"

        x :: y :: xs ->
            case build_meta_from_flags x y of
                Nothing ->
                    fail ""

                Just value ->
                    succeed value


build_meta_from_flags : EntryFlags -> EntryFlags -> Maybe Record
build_meta_from_flags idEntry valueEntry =
    let
        maybeId =
            case idEntry of
                EntryId string ->
                    Just string

                _ ->
                    Nothing

        maybeEntry =
            case valueEntry of
                EntryValue value ->
                    Just value

                _ ->
                    Nothing
    in
    case ( maybeId, maybeEntry ) of
        ( Just id, Just entries ) ->
            Just ( id, entries )

        _ ->
            Nothing


recordDecoder : Decoder Record
recordDecoder =
    list (oneOf [ map EntryId string, map EntryValue (list content_node) ])
        |> andThen buildRecord


section : Decoder content -> Decoder (Section_ content)
section c =
    map3
        Section_
        (field "frontmatter" (frontmatter c))
        (field "mainmatter" c)
        (field "flags" section_flags)


type alias Article content =
    { frontmatter : Frontmatter content
    , mainmatter : content
    , backmatter : content
    }


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
            (map2 Full
                (field "Section_flags" section_flags)
                (field
                    "Frontmatter_overrides"
                    (frontmatter_overrides c)
                )
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
    map3 Transclusion
        (field "addr" addr)
        (field "target" (content_target c))
        (field "modifier" modifier)


type alias Link_ content =
    { href : String, content : content }


link : Decoder content -> Decoder (Link_ content)
link c =
    map2
        Link_
        (field "href" string)
        (field "content" c)


type alias Inline_img =
    { format : String, base64 : String }


inline_img : Decoder Inline_img
inline_img =
    map2 Inline_img (field "format" string) (field "base64" string)


type alias Resource_source =
    { type_ : String, part : String, source : String }


resource_source : Decoder Resource_source
resource_source =
    map3 Resource_source
        (field "type_" string)
        (field "parts" string)
        (field "source" string)


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


content_node : Decoder Content_node
content_node =
    oneOf
        [ field "Text" string |> map Text
        , field "CDATA" string |> map CDATA
        , field "Xml_elt" (xml_elt content) |> map Xml_elt
        , field "Transclude" (transclusion content) |> map Transclude
        , field "Results_of_query" (Query.expr int) |> map Results_of_query
        , field "Section" (section content) |> map Section
        , field "Prim"
            -- interesting, it's curried
            (prim
                |> andThen
                    (\p ->
                        content
                            |> map (\c -> Prim ( p, c ))
                    )
            )
        , field "KaTeX"
            (map2 KaTeX
                (field "Math_mode" Base.math_mode)
                (field "Content" content)
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
    field "Content" (list (lazy (\_ -> content_node))) |> map Content


type alias Resource_ =
    { hash : String, content : List Content_node, sources : List Resource_source }


resource : Decoder Resource_
resource =
    map3 Resource_
        (field "hash" string)
        (field "content" (list (lazy (\_ -> content_node))))
        (field "sources" (list resource_source))


type alias Model =
    List Record


decoder : Decoder Model
decoder =
    list recordDecoder


jsonString : String
jsonString =
    "[a, \"foo\", b, \"bar\"]"


init : Model
init =
    case decodeString decoder jsonString of
        Ok res ->
            res

        Err _ ->
            []


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Msg =
    ()


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    div []
        [ Html.text <| Debug.toString model ]
