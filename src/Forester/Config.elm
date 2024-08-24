module Forester.Config exposing (..)

import Json.Decode exposing (Decoder, list, string, succeed)
import Json.Decode.Pipeline exposing (optional)


type alias Config =
    { trees : List String
    , assets : List String
    , theme : String
    , stylesheet : String
    , root : String
    }


config : Decoder Config
config =
    succeed Config
        |> optional "trees" (list string) []
        |> optional "assets" (list string) []
        |> optional "theme" string ""
        |> optional "stylesheet" string ""
        |> optional "root" string ""
