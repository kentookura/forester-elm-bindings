module Prelude exposing (Date, date)

import Json.Decode exposing (Decoder, field, int, map3, maybe)


type alias Date =
    { yyyy : Int, mm : Maybe Int, dd : Maybe Int }


date : Decoder Date
date =
    map3 Date
        (field "yyyy" int)
        (field "mm" (maybe int))
        (field "dd" (maybe int))
