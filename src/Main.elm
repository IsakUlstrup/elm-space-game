module Main exposing (..)

import Browser
import Browser.Events
import ComponentData exposing (buffCompData, colorCompData, skillCompData, statCompData)
import Components.Buff
import Components.Color
import Components.Meter exposing (newMeter)
import Components.Skill
import Components.Stat
import Ecs
import GameData exposing (GameMsg, GameScene)
import Html exposing (Html, div)
import Systems.BuffSystem exposing (buffSystem)
import Systems.SkillSystem exposing (skillSystem)
import View


type Msg
    = Tick Float
    | GameMsg GameMsg



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
            , skillCompData (Components.Skill.newSkill 5000 "Super skill" "The best skill" |> Components.Skill.resetCooldown)
            , colorCompData Components.Color.initColor
            ]
        |> Ecs.addEntity [ statCompData (Components.Stat.shieldStat 15 |> Components.Stat.reduceStatValue 8) ]
        |> Ecs.addEntity [ statCompData (Components.Stat.hullStat 30) ]
        |> Ecs.addEntity [ skillCompData (Components.Skill.newSkill 5000 "Super skill" "The best skill") ]
        |> Ecs.addEntity
            [ statCompData (Components.Stat.hullStat 3)
            , statCompData (Components.Stat.powerStat 3)
            , buffCompData (Components.Buff.newBuff "Power buff" "" [ Components.Stat.powerStat 3 ] (Just (newMeter 10000 10000)))
            , colorCompData
                (Components.Color.initColor
                    |> Components.Color.withHue 120
                    |> Components.Color.withLightness 40
                )
            ]
        |> Ecs.addEntity [ buffCompData (Components.Buff.newBuff "Power buff" "" [ Components.Stat.powerStat 3 ] (Just (newMeter 1000 1000))) ]
        |> Ecs.addEntity [ buffCompData (Components.Buff.newBuff "Power buff" "" [ Components.Stat.powerStat 3 ] Nothing) ]
        |> Ecs.addSystem skillSystem
        |> Ecs.addSystem buffSystem
    , Cmd.none
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( model |> Ecs.runSystems (GameData.GameTick dt), Cmd.none )

        GameMsg gameMsg ->
            ( model |> Ecs.runSystems gameMsg, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Html.map GameMsg (View.viewScene model)
        ]



---- SUBSCRIPTIONS ----


subs : Model -> Sub Msg
subs _ =
    Browser.Events.onAnimationFrameDelta Tick



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subs
        }
