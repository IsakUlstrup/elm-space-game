module Main exposing (..)

import Browser
import ComponentData exposing (statCompData)
import Components.Stat
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
        |> Ecs.addEntity
            [ statCompData (Components.Stat.powerStat 3)
            , statCompData (Components.Stat.hullStat 3)
            , statCompData (Components.Stat.powerStat 3 |> Components.Stat.reduceStatValue 2)
            ]
        |> Ecs.addEntity [ statCompData (Components.Stat.shieldStat 15 |> Components.Stat.reduceStatValue 8) ]
        |> Ecs.addEntity [ statCompData (Components.Stat.hullStat 30) ]
        |> Ecs.addEntity []
        |> Ecs.addEntity
            [ statCompData (Components.Stat.hullStat 3)
            , statCompData (Components.Stat.powerStat 3)
            ]
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



---- SUBSCRIPTIONS ----


subs : Model -> Sub Msg
subs _ =
    Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subs
        }
