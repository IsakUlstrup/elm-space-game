module Main exposing (..)

import Browser
import Ecs
import GameData exposing (GameScene)
import Html exposing (Html, div)
import View


type Msg
    = NoOp



---- MODEL ----


type alias Model =
    GameScene


init : ( Model, Cmd Msg )
init =
    ( Ecs.emptyScene 0
        |> Ecs.addEntity []
        |> Ecs.addEntity []
        |> Ecs.addEntity []
        |> Ecs.addEntity []
        |> Ecs.addEntity []
    , Cmd.none
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ View.viewScene model
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
