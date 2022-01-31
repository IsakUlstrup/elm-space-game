module Main exposing (..)

import Browser
import Browser.Events
import ComponentData exposing (skillCompData, statCompData)
import Components.Buff exposing (Buff)
import Components.Meter exposing (newMeter)
import Components.Skill
import Components.Stat
import Ecs
import GameData exposing (GameMsg, GameScene)
import Html exposing (Html)
import Systems.BuffSystem exposing (buffSystem)
import Systems.SkillSystem exposing (skillSystem)
import View


skillBuff : Buff
skillBuff =
    Components.Buff.newBuff
        "Stasis"
        "Stops cooldown recovery"
        [ Components.Stat.cooldownRecoveryStat 3 |> Components.Stat.applyModifiers [ Components.Stat.setSubModifier 0 ] ]
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
            [ statCompData (Components.Stat.powerStat 1)
            , skillCompData
                (Components.Skill.newSkill 1000
                    "EMP Blast"
                    "Stops target cooldown recovery"
                    (Components.Skill.buffEffect skillBuff)
                )
            ]
        --     [ statCompData (Components.Stat.powerStat 3)
        --     , statCompData (Components.Stat.hullStat 3)
        --     , statCompData (Components.Stat.powerStat 3 |> Components.Stat.reduceStatValue 2)
        --     , skillCompData (Components.Skill.newSkill 5000 "Super skill" "The best skill" (Components.Skill.damageEffect 10) |> Components.Skill.resetCooldown)
        --     , colorCompData Components.Color.initColor
        --     ]
        -- |> Ecs.addEntity [ statCompData (Components.Stat.shieldStat 15 |> Components.Stat.reduceStatValue 8) ]
        -- |> Ecs.addEntity [ statCompData (Components.Stat.hullStat 30) ]
        -- |> Ecs.addEntity
        --     [ skillCompData
        --         (Components.Skill.newSkill 1000
        --             "Buff skill"
        --             "buffs power"
        --             (Components.Skill.buffEffect skillBuff)
        --         )
        --     ]
        -- |> Ecs.addEntity
        --     [ statCompData (Components.Stat.hullStat 3)
        --     , statCompData (Components.Stat.powerStat 3)
        --     , buffCompData (Components.Buff.newBuff "Power buff" "Buffs power stat" [ Components.Stat.powerStat 3 ] (Just (newMeter 10000 10000)))
        --     , colorCompData
        --         (Components.Color.initColor
        --             |> Components.Color.withHue 40
        --             |> Components.Color.withLightness 40
        --         )
        --     ]
        -- |> Ecs.addEntity [ buffCompData (Components.Buff.newBuff "Power buff" "Buffs power stat" [ Components.Stat.powerStat 3 ] (Just (newMeter 1000 1000))) ]
        -- |> Ecs.addEntity [ buffCompData (Components.Buff.newBuff "Power buff" "Buffs power stat" [ Components.Stat.powerStat 3 ] Nothing) ]
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
