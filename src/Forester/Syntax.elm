module Forester.Syntax exposing (Node(..), Tree)

import Forester.Base exposing (Binding, Delim, MathMode, Visibility)


type alias Object_ =
    { self : Maybe (List String), methods : List ( String, List Node ) }


type alias Patch_ =
    { obj : List Node, self : Maybe (List String), methods : List ( String, List Node ) }


type Node
    = Text String
    | Verbatim String
    | Group Delim (List Node)
    | Math MathMode (List Node)
    | Ident (List String) (List String)
    | XmlTag ( Maybe String, String ) (List ( ( Maybe String, String ), List Node )) (List Node)
    | Subtree (Maybe String) (List Node)
    | Let (List String) (List (Binding (List String))) (List Node)
    | Open (List String)
    | Scope (List Node)
    | Put (List String) (List Node)
    | Default (List String) (List Node)
    | Get (List String)
    | Fun (List (Binding (List String))) (List Node)
    | Object Object_
    | Patch Patch_
    | Call (List Node) String
    | Import Visibility String
    | Def (List String) (List (Binding (List String))) (List Node)
    | DeclXmlns String String
    | Alloc (List String)
    | Namespace (List String) (List Node)


type alias Tree =
    { sourcePath : Maybe String, addr : Maybe String, code : List Node }
