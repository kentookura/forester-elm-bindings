module Forester.Parser exposing
    ( alloc
    , arg
    , binder
    , binderHelp
    , binders
    , bvar
    , call
    , codeExpr
    , declXmlns
    , def
    , default
    , display
    , export
    , file
    , fun
    , funSpec
    , get
    , ident
    , identWithMethodCalls
    , import_
    , inline
    , let_
    , list
    , methodDecl
    , namespace
    , node
    , object
    , open
    , patch
    , put
    , scope
    , subtree
    , textualExpr
    , textualNode
    , verbatim
    , xmlAttr
    , xmlBaseIdent
    , xmlQname
    , xmlTag
    )

-- Since I expect to use this in an editing frontend, I don't really need this
-- to be so efficient that it can parse hundreds of trees.

import Forester.Base exposing (Binding, BindingStrategy(..), Delim(..), MathMode(..), Visibility(..))
import Forester.Parser.Utils
    exposing
        ( braces
        , delimited
        , option
        , parens
        , squares
        , text
        , txtArg
        , wstext
        )
import Forester.Syntax as Syntax exposing (Node(..))
import Forester.XmlTree exposing (Modifier(..))
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , chompWhile
        , getChompedString
        , lazy
        , loop
        , map
        , oneOf
        , succeed
        , symbol
        )


list : Parser a -> Parser (List a)
list p =
    let
        aux : List a -> Parser (Step (List a) (List a))
        aux rev =
            oneOf
                [ succeed (\n -> Loop (n :: rev))
                    |= p
                , succeed ()
                    |> map (\_ -> Done (List.reverse rev))
                ]
    in
    loop [] aux


def : Parser Syntax.Node
def =
    succeed identity
        |. symbol "\\def"
        |= (funSpec
                |> andThen (\( i, b, a ) -> succeed (Def i b a))
           )


alloc : Parser Syntax.Node
alloc =
    succeed Alloc
        |. symbol "\\alloc"
        |= ident


import_ : Parser Syntax.Node
import_ =
    succeed identity
        |. symbol "\\import"
        |= (txtArg |> andThen (\str -> succeed (Import Private str)))


export : Parser Syntax.Node
export =
    succeed identity
        |. symbol "\\export"
        |= (txtArg |> andThen (\str -> succeed (Import Public str)))


namespace : Parser Syntax.Node
namespace =
    succeed Namespace
        |. symbol "\\namespace"
        |= ident
        |= braces codeExpr


subtree : Parser Syntax.Node
subtree =
    succeed Subtree
        |. symbol "\\subtree"
        |= option (delimited Squares wstext)
        |= arg


fun : Parser Syntax.Node
fun =
    succeed Fun
        |= binders
        |= arg


let_ : Parser Syntax.Node
let_ =
    succeed identity
        |. symbol "\\let"
        |= (funSpec
                |> andThen (\( i, b, a ) -> succeed (Let i b a))
           )


ident : Parser (List String)
ident =
    succeed identity
        |. symbol "\\"
        |= (getChompedString <|
                succeed ()
                    |. chompWhile (\c -> Char.isAlpha c || Char.isDigit c || c == '-' || c == '/' || c == '#')
           )
        |> andThen (\str -> succeed (String.split "/" str))


scope : Parser Syntax.Node
scope =
    succeed Scope
        |. symbol "\\scope"
        |= arg


put : Parser Syntax.Node
put =
    succeed Put
        |. symbol "\\put"
        |= ident
        |= arg


default : Parser Syntax.Node
default =
    succeed Default
        |. symbol "\\put?"
        |= ident
        |= arg


get : Parser Syntax.Node
get =
    succeed Get
        |. symbol "\\get"
        |= ident


open : Parser Syntax.Node
open =
    succeed Open
        |. symbol "\\open"
        |= ident


xmlAttr : Parser ( ( Maybe String, String ), List Node )
xmlAttr =
    let
        splitXmlQname str =
            case String.split ":" str of
                [ prefix, uname ] ->
                    ( Just prefix, uname )

                [ uname ] ->
                    ( Nothing, uname )

                _ ->
                    Debug.todo "Failed to split xml qname"
    in
    squares text
        |> andThen
            (\k ->
                arg
                    |> andThen (\v -> succeed ( splitXmlQname k, v ))
            )


xmlBaseIdent : Parser String
xmlBaseIdent =
    getChompedString <| succeed () |. chompWhile (\c -> Char.isAlpha c || Char.isDigit c || c == '-' || c == '_')


xmlQname : Parser ( Maybe String, String )
xmlQname =
    oneOf
        [ succeed (\str -> ( Nothing, str ))
            |= xmlBaseIdent
        , succeed (\a b -> ( Just a, b ))
            |= xmlBaseIdent
            |. symbol ":"
            |= xmlBaseIdent
        ]


xmlTag : Parser Syntax.Node
xmlTag =
    succeed XmlTag
        |. symbol "\\<"
        |= xmlQname
        |= list xmlAttr
        |= arg


declXmlns : Parser Syntax.Node
declXmlns =
    succeed DeclXmlns
        |. symbol "\\xmlns:"
        |= xmlBaseIdent
        |= txtArg


object : Parser Syntax.Node
object =
    succeed identity
        |. symbol "\\object"
        |= option (squares bvar)
        |> andThen
            (\slf ->
                braces (list methodDecl)
                    |> andThen
                        (\methods -> succeed (Object { self = slf, methods = methods }))
            )


methodDecl : Parser ( String, List Node )
methodDecl =
    squares text
        |> andThen
            (\k ->
                arg
                    |> andThen (\v -> succeed ( k, v ))
            )


bvar : Parser (List String)
bvar =
    text |> andThen (\x -> succeed [ x ])


patch : Parser Syntax.Node
patch =
    succeed Patch
        |. symbol "\\patch"
        |= (braces codeExpr
                |> andThen
                    (\obj ->
                        option (squares bvar)
                            |> andThen
                                (\self ->
                                    braces
                                        (list methodDecl
                                            |> andThen (\methods -> succeed { obj = obj, self = self, methods = methods })
                                        )
                                )
                    )
           )


call : Parser Syntax.Node
call =
    succeed Call
        |. symbol "\\call"
        |= braces codeExpr
        |= txtArg


inline : Parser Syntax.Node
inline =
    succeed identity
        |. symbol "#{"
        |= lazy
            (\_ ->
                list node
                    |> andThen (\nodes -> succeed (Math Inline nodes))
            )
        |. symbol "}"


display : Parser Syntax.Node
display =
    succeed identity
        |. symbol "##{"
        |= lazy
            (\_ ->
                list node
                    |> andThen (\nodes -> succeed (Math Display nodes))
            )
        |. symbol "}"


verbatim : Parser Syntax.Node
verbatim =
    succeed Verbatim
        |. symbol "\\verb"
        |= text


arg =
    oneOf
        []


node : Parser Syntax.Node
node =
    oneOf
        [ def
        , alloc
        , import_
        , export
        , namespace
        , subtree
        , fun
        , let_
        , identWithMethodCalls
        , scope
        , put
        , default
        , get
        , open
        , xmlTag
        , declXmlns
        , object
        , patch
        , call
        , inline
        , display
        , succeed identity
            |= braces textualExpr
            |> andThen (\nodes -> succeed (Group Braces nodes))
        , succeed identity
            |= squares textualExpr
            |> andThen (\nodes -> succeed (Group Squares nodes))
        , succeed identity
            |= parens textualExpr
            |> andThen (\nodes -> succeed (Group Parens nodes))
        , verbatim
        ]


funSpec : Parser ( List String, List (Binding (List String)), List Node )
funSpec =
    ident
        |> andThen
            (\i ->
                binders
                    |> andThen
                        (\b ->
                            arg
                                |> andThen (\a -> succeed ( i, b, a ))
                        )
            )


codeExpr : Parser (List Node)
codeExpr =
    lazy (\_ -> list node)


binder : Parser (Binding (List String))
binder =
    succeed identity
        |= text
        |> andThen
            (\str ->
                case String.uncons str of
                    Just ( '~', rest ) ->
                        succeed ( Lazy, [ rest ] )

                    _ ->
                        succeed ( Strict, [ str ] )
            )


binders : Parser (List (Binding (List String)))
binders =
    loop [] binderHelp


binderHelp : List (Binding (List String)) -> Parser (Step (List (Binding (List String))) (List (Binding (List String))))
binderHelp revBinders =
    oneOf
        [ succeed (\b -> Loop (b :: revBinders))
            |= binder
        , succeed
            ()
            |> map (\_ -> Done (List.reverse revBinders))
        ]


identWithMethodCalls : Parser Node
identWithMethodCalls =
    succeed identity
        |. symbol "\\"
        |= (getChompedString
                (succeed ()
                    |. chompWhile Char.isAlpha
                )
                |> andThen
                    (\str ->
                        let
                            ( a, b ) =
                                case String.split "#" str of
                                    [ x ] ->
                                        ( String.split "/" x, [] )

                                    "" :: xs ->
                                        ( [ "#" ], List.filter (\x -> x /= "") xs )

                                    x :: xs ->
                                        ( String.split "/" x, List.filter (\y -> y /= "") xs )

                                    _ ->
                                        ( [], [] )
                        in
                        succeed (Ident a b)
                    )
           )


textualExpr : Parser (List Node)
textualExpr =
    list textualNode


textualNode : Parser Node
textualNode =
    oneOf
        [ succeed Text |= text
        , lazy (\_ -> node)
        ]


file : Parser (List Node)
file =
    let
        aux : List Node -> Parser (Step (List Node) (List Node))
        aux revNodes =
            oneOf
                [ succeed (\n -> Loop (n :: revNodes))
                    |= node
                , succeed () |> map (\_ -> Done (List.reverse revNodes))
                ]
    in
    loop [] aux
