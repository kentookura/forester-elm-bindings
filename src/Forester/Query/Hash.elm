module Forester.Query.Hash exposing (hashQuery)

import Forester.Base exposing (Addr(..))
import Forester.Query exposing (AddrExpr(..), Dbix, Expr(..), Mode(..), Polarity(..))
import Hash exposing (Hash)


hashMode : Mode -> Hash
hashMode m =
    case m of
        Edges ->
            Hash.fromString "Edges"

        Paths ->
            Hash.fromString "Paths"


hashPolarity : Polarity -> Hash
hashPolarity m =
    case m of
        Incoming ->
            Hash.fromString "Incoming"

        Outgoing ->
            Hash.fromString "Outgoing"


hashAddr : Addr -> Hash
hashAddr a =
    case a of
        UserAddr str ->
            Hash.dependent (Hash.fromString "UserAddr") (Hash.fromString str)

        MachineAddr ix ->
            Hash.dependent (Hash.fromString "MachineAddr") (Hash.fromInt ix)

        HashAddr str ->
            Hash.dependent (Hash.fromString "HashAddr") (Hash.fromString str)

        Anon ->
            Hash.fromString "Anon"


hashAddrExpr : (var -> Hash) -> AddrExpr var -> Hash
hashAddrExpr var e =
    case e of
        Addr a ->
            Hash.dependent (Hash.fromString "Addr") (hashAddr a)

        Var v ->
            Hash.dependent (Hash.fromString "Var") (var v)


hashQuery : Expr Dbix -> Hash
hashQuery expr =
    case expr of
        Rel m p r e ->
            List.foldl Hash.dependent
                (Hash.fromString "")
                [ Hash.fromString "Rel"
                , hashMode m
                , hashPolarity p
                , Hash.fromString r
                , hashAddrExpr Hash.fromInt e
                ]

        Isect _ ->
            Hash.fromString "Isect"

        Union _ ->
            Hash.fromString "Union"

        Complement _ ->
            Hash.fromString "Complement"

        UnionFam _ _ ->
            Hash.fromString "UnionFam"

        IsectFam _ _ ->
            Hash.fromString "IsectFam"
