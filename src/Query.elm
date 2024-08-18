module Query exposing
    ( Addr_expr(..)
    , Dbix
    , Expr(..)
    , Mode(..)
    , Polarity(..)
    , dbix
    , expr
    )

import Base exposing (addr)
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
        , map2
        , map4
        , oneOf
        , string
        , succeed
        )


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


type Addr_expr var
    = Addr Base.Addr
    | Var var


addr_expr : Decoder var -> Decoder (Addr_expr var)
addr_expr v =
    oneOf [ field "Addr" (Base.addr |> map Addr), field "Var" v |> map Var ]


type alias Dbix =
    Int


dbix : Decoder Dbix
dbix =
    int


type Expr var
    = Rel Mode Polarity Rel (Addr_expr var)
    | Isect (List (Expr var))
    | Union (List (Expr var))
    | Complement (Expr var)
    | Union_fam (Expr var) (Binder (Expr var))
    | Isect_fam (Expr var) (Binder (Expr var))


type Relpart var
    = M Mode
    | P Polarity
    | R Rel
    | AE (Addr_expr var)


relpart : Decoder var -> Decoder (Relpart var)
relpart var =
    oneOf
        [ mode |> map M
        , polarity |> map P
        , string |> map R
        , addr_expr var |> map AE
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
        , field "Union_fam"
            (map2 Union_fam
                (field "Expr" (lazy (\_ -> expr var)))
                (field "Binder" (binder (lazy (\_ -> expr var))))
            )
        , field "Isect_fam"
            (map2 Isect_fam
                (field "Expr" (lazy (\_ -> expr var)))
                (field "Binder" (binder (lazy (\_ -> expr var))))
            )
        ]
