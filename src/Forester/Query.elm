module Forester.Query exposing
    ( AddrExpr(..)
    , Dbix
    , Expr(..)
    , Mode(..)
    , Polarity(..)
    , dbix
    , encodeAddr
    , encodeExpr
    , expr
    )

import Forester.Base exposing (Addr(..), addr)
import Html exposing (wbr)
import Json.Decode
    exposing
        ( Decoder
        , andThen
        , fail
        , field
        , int
        , lazy
        , list
        , map
        , oneOf
        , string
        , succeed
        )
import Json.Encode as Encode exposing (Value)


type alias Rel =
    String


type Mode
    = Edges
    | Paths


mode : Decoder Mode
mode =
    string
        |> andThen
            (\str ->
                case str of
                    "Edges" ->
                        succeed Edges

                    "Paths" ->
                        succeed Paths

                    _ ->
                        fail "Failed to decode Mode"
            )


type Polarity
    = Incoming
    | Outgoing


polarity : Decoder Polarity
polarity =
    string
        |> andThen
            (\str ->
                case str of
                    "Incoming" ->
                        succeed Incoming

                    "Outgoing" ->
                        succeed Outgoing

                    _ ->
                        fail "Failed to decode polarity"
            )


type alias Binder a =
    { body : a }


binder : Decoder a -> Decoder (Binder a)
binder a =
    map Binder (field "body" a)


type AddrExpr var
    = Addr Addr
    | Var var


addrExpr : Decoder var -> Decoder (AddrExpr var)
addrExpr v =
    oneOf [ field "Addr" (addr |> map Addr), field "Var" v |> map Var ]


type alias Dbix =
    Int


dbix : Decoder Dbix
dbix =
    int


type Expr var
    = Rel Mode Polarity Rel (AddrExpr var)
    | Isect (List (Expr var))
    | Union (List (Expr var))
    | Complement (Expr var)
    | UnionFam (Expr var) (Binder (Expr var))
    | IsectFam (Expr var) (Binder (Expr var))


type FamPart var
    = E (Expr var)
    | B (Binder (Expr var))


fampart : Decoder var -> Decoder (FamPart var)
fampart v =
    oneOf
        [ expr v |> map E
        , binder (expr v) |> map B
        ]


fam : (Expr var -> Binder (Expr var) -> a) -> Decoder var -> Decoder a
fam famkind v =
    list (fampart v)
        |> andThen
            (\parts ->
                case parts of
                    [ E e, B b ] ->
                        succeed (famkind e b)

                    _ ->
                        fail ""
            )


type Relpart var
    = M Mode
    | P Polarity
    | R Rel
    | AE (AddrExpr var)


relpart : Decoder var -> Decoder (Relpart var)
relpart var =
    oneOf
        [ mode |> map M
        , polarity |> map P
        , string |> map R
        , addrExpr var |> map AE
        ]


rel : Decoder var -> Decoder (Expr var)
rel v =
    list (relpart v)
        |> andThen
            (\parts ->
                case parts of
                    [ M m, P pol, R r, AE e ] ->
                        succeed (Rel m pol r e)

                    _ ->
                        fail "Failed to decode Rel"
            )


expr : Decoder var -> Decoder (Expr var)
expr var =
    oneOf
        [ field "Rel" (rel var)
        , field "Isect" (list (lazy (\_ -> expr var)) |> map Isect)
        , field "Union" (list (lazy (\_ -> expr var)) |> map Union)
        , field "Complement" (lazy (\_ -> expr var) |> map Complement)
        , field "Union_fam" (lazy (\_ -> fam UnionFam var))
        , field "Isect_fam" (lazy (\_ -> fam IsectFam var))
        ]


encodeAddr : Addr -> Value
encodeAddr a =
    case a of
        UserAddr str ->
            Encode.object [ ( "User_addr", Encode.string str ) ]

        MachineAddr i ->
            Encode.object [ ( "Machine_addr", Encode.int i ) ]

        HashAddr hash ->
            Encode.object [ ( "Hash_addr", Encode.string hash ) ]

        Anon ->
            Encode.string "Anon"


encodeAddrExpr : (v -> Value) -> AddrExpr v -> Value
encodeAddrExpr encodeVar e =
    case e of
        Addr addr ->
            Encode.object [ ( "Addr", encodeAddr addr ) ]

        Var v ->
            Encode.object [ ( "Var", encodeVar v ) ]


encodeRelpart : (var -> Value) -> Relpart var -> Value
encodeRelpart varEncoder rp =
    case rp of
        M m ->
            case m of
                Edges ->
                    Encode.string "Edges"

                Paths ->
                    Encode.string "Paths"

        P p ->
            case p of
                Incoming ->
                    Encode.string "Incoming"

                Outgoing ->
                    Encode.string "Outgoing"

        R r ->
            Encode.string r

        AE a ->
            encodeAddrExpr varEncoder a


encodeFamPart : (var -> Value) -> FamPart var -> Value
encodeFamPart varEncoder fp =
    case fp of
        E e ->
            encodeExpr varEncoder e

        B b ->
            encodeBinder varEncoder b


encodeBinder : (var -> Value) -> Binder (Expr var) -> Value
encodeBinder varEncoder { body } =
    Encode.object [ ( "body", encodeExpr varEncoder body ) ]


encodeExpr : (v -> Value) -> Expr v -> Value
encodeExpr valEncoder e =
    case e of
        Rel m p r ae ->
            Encode.object
                [ ( "Rel"
                  , Encode.list (\rp -> encodeRelpart valEncoder rp)
                        [ M m
                        , P p
                        , R r
                        , AE ae
                        ]
                  )
                ]

        Isect exprs ->
            Encode.object
                [ ( "Isect"
                  , Encode.list (encodeExpr valEncoder) exprs
                  )
                ]

        Union exprs ->
            Encode.object
                [ ( "Union"
                  , Encode.list (encodeExpr valEncoder) exprs
                  )
                ]

        Complement xpr ->
            Encode.object
                [ ( "Complement"
                  , encodeExpr valEncoder xpr
                  )
                ]

        UnionFam xpr b ->
            Encode.object
                [ ( "Union_fam"
                  , Encode.list (\fp -> encodeFamPart valEncoder fp)
                        []
                  )
                ]

        IsectFam xpr b ->
            Encode.object
                [ ( "Isect_fam"
                  , Encode.list (\fp -> encodeFamPart valEncoder fp)
                        []
                  )
                ]
