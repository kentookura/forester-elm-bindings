module Main exposing (main)

import Browser
import Html exposing (Html, pre)
import Json.Decode exposing (decodeString, errorToString)
import Xml_tree exposing (content, metas)


type alias Model =
    String


jsonString : String
jsonString =
    """
    [ "external", [ { "Text": "asdf" } ] ]
    """


init : Model
init =
    case decodeString (metas content) jsonString of
        Ok res ->
            res |> Debug.toString

        Err err ->
            err |> errorToString


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
    pre []
        [ Html.text <| Debug.toString model ]
