module Main exposing (..)

import Browser
import Browser.Events
import ComponentData exposing (partCompData)
import Components.Buff exposing (Buff)
import Components.Meter exposing (newMeter)
import Components.Part exposing (newPart)
import Components.Skill
import Components.Stat
import Ecs
import GameData exposing (GameMsg, GameScene)
import Html exposing (Html)
import Systems.BuffSystem exposing (buffSystem)
import Systems.DeathSystem exposing (deathSystem)
import Systems.SkillSystem exposing (skillSystem)
import View


skillBuff : Buff
skillBuff =
    Components.Buff.newBuff
        "Stasis"
        "Stops cooldown recovery"
        [ Components.Stat.cooldownRecoveryStat 3 |> Components.Stat.addModifiers [ Components.Stat.setSubModifier 0 ] ]
        (Just (newMeter 5000 5000))


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
            [ partCompData
                (newPart "Test part"
                    [ Components.Skill.newSkill 1000
                        "EMP"
                        "disables skill cooldown recovery"
                        (Components.Skill.buffEffect skillBuff)
                    , Components.Skill.newSkill 1000
                        "Laser"
                        "dps"
                        (Components.Skill.damageEffect 10)
                    ]
                    [ skillBuff
                    ]
                )
            ]
        |> Ecs.addEntity []
        |> Ecs.addEntity []
        |> Ecs.addEntity []
        |> Ecs.addEntity
            [ partCompData
                (newPart "Test part"
                    [ Components.Skill.newSkill 1000
                        "Buff skill"
                        "buffs power"
                        (Components.Skill.buffEffect skillBuff)
                    ]
                    [ Components.Buff.newBuff
                        "Stasis"
                        "Stops cooldown recovery"
                        [ Components.Stat.cooldownRecoveryStat 3 |> Components.Stat.addModifiers [ Components.Stat.setSubModifier 0 ] ]
                        Nothing
                    ]
                )
            ]
        |> Ecs.addSystem skillSystem
        |> Ecs.addSystem buffSystem
        |> Ecs.addSystem deathSystem
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
    Html.map GameMsg (View.viewScene model)



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
