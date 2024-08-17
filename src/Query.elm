module Query exposing (Dbix, Expr, dbix, expr)

import Base
import Json.Decode exposing (Decoder, andThen, fail, field, int, lazy, list, map, map2, map4, oneOf, string, succeed)


type alias Rel =
    String


rel : Decoder Rel
rel =
    string


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



-- TODO: Lazify


expr : Decoder var -> Decoder (Expr var)
expr var =
    oneOf
        [ field "Rel"
            (map4 Rel
                (field "Mode" mode)
                (field "Polarity" polarity)
                (field "Rel" rel)
                (field "Addr_expr" (addr_expr var))
            )
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
