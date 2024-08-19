module Forester.Parser exposing (file)

-- Since I expect to use this in an editing frontend, I don't really need this
-- to be so efficient that it can parse hundreds of trees.

import Forester.Base exposing (Binding, BindingStrategy(..), Delim(..), MathMode(..), Visibility(..), delimToString)
import Forester.Syntax as Syntax exposing (Node(..))
import Forester.XmlTree exposing (XmlAttr)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , chompWhile
        , float
        , getChompedString
        , lazy
        , loop
        , map
        , oneOf
        , run
        , spaces
        , succeed
        , symbol
        )
import Tuple exposing (first, second)


delimited : Delim -> Parser a -> Parser a
delimited d p =
    let
        pair =
            delimToString d
    in
    -- This is wrong
    succeed identity
        |. symbol (first pair)
        |= p
        |. symbol (second pair)


option : Parser a -> Parser (Maybe a)
option p =
    oneOf [ succeed Just |= p, succeed Nothing ]


list : Parser a -> Parser (List a)
list p =
    let
        aux : List a -> Parser (Step (List a) (List a))
        aux rev =
            oneOf
                [ succeed (\n -> Loop (n :: rev))
                    |= p
                , succeed () |> map (\_ -> Done (List.reverse rev))
                ]
    in
    loop [] aux


text : Parser String
text =
    getChompedString <|
        succeed ()
            |. chompWhile
                (\c ->
                    List.any (\b -> c == b)
                        [ ' '
                        , '%'
                        , '#'
                        , '\\'
                        , '{'
                        , '}'
                        , '['
                        , ']'
                        , '('
                        , ')'
                        , '\u{000D}'
                        , '\n'
                        ]
                )


def : Parser Syntax.Node
def =
    succeed identity
        |. symbol "\\def"
        |= (funSpec
                |> andThen (\( i, b, a ) -> succeed (Def i b a))
           )


alloc : Parser Syntax.Node
alloc =
    succeed Alloc |= ident


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
        |= ident
        |= braces codeExpr


subtree : Parser Syntax.Node
subtree =
    succeed Subtree
        |= option (delimited Squares wstext)
        |= braces (Debug.todo "foo")


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
        |= arg


put : Parser Syntax.Node
put =
    succeed Put
        |= ident
        |= arg


default : Parser Syntax.Node
default =
    succeed Default
        |= ident
        |= arg


get : Parser Syntax.Node
get =
    succeed Get
        |= ident


open : Parser Syntax.Node
open =
    succeed Open
        |. symbol "\\open"
        |= ident


xmlAttr : Parser ( ( Maybe String, String ), List Node )
xmlAttr =
    Debug.todo "implement xmlattr"



-- succeed XmlAttr
--     |= squares text
--     |= arg


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
    Debug.todo "declXmlns"


object : Parser Syntax.Node
object =
    Debug.todo "implement object"


patch : Parser Syntax.Node
patch =
    Debug.todo "implement patch"


call : Parser Syntax.Node
call =
    Debug.todo "implement call"


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


braces : Parser a -> Parser a
braces p =
    delimited Braces p


squares : Parser a -> Parser a
squares p =
    delimited Squares p


parens : Parser a -> Parser a
parens p =
    delimited Parens p


verbatim : Parser Syntax.Node
verbatim =
    Debug.todo "implement verbatim"


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
        , braces textualExpr
        , squares textualExpr
        , parens textualExpr
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


txtArg : Parser String
txtArg =
    braces wstext


wstext : Parser String
wstext =
    getChompedString <|
        succeed ()
            |. chompWhile (\c -> c /= ' ')


codeExpr : Parser a
codeExpr =
    Debug.todo "implement codeExpr"


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


textualExpr : Parser a
textualExpr =
    Debug.todo "textual expr"


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
