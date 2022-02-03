module Main exposing (..)

import Browser
import Browser.Events
import ComponentData exposing (coreCompData, partCompData)
import Components.Core
import Components.Part exposing (newPart)
import Content.Buffs
import Content.Skills
import Ecs
import GameData exposing (GameMsg, GameScene)
import Html exposing (Html)
import Systems.BuffSystem exposing (buffSystem)
import Systems.DeathSystem exposing (deathSystem)
import Systems.SkillSystem exposing (skillSystem)
import View.View


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
                (newPart "Super part"
                    [ Content.Skills.emp
                    , Content.Skills.laser
                    , Content.Skills.speedBoost
                    ]
                    [ Content.Buffs.stasis (Just 5000) ]
                )
            , partCompData
                (newPart "Super part"
                    [ Content.Skills.emp
                    , Content.Skills.laser
                    , Content.Skills.speedBoost
                    ]
                    [ Content.Buffs.stasis (Just 5000) ]
                )
            , coreCompData (Components.Core.newPlayerCore True)
            ]
        |> Ecs.addEntity []
        |> Ecs.addEntity
            [ partCompData
                (newPart "Laser"
                    [ Content.Skills.laser ]
                    []
                )
            , coreCompData Components.Core.newFactionCore
            ]
        |> Ecs.addEntity []
        |> Ecs.addEntity
            [ partCompData
                (newPart "Laser/Stasis"
                    [ Content.Skills.laser ]
                    [ Content.Buffs.stasis Nothing ]
                )
            , coreCompData Components.Core.newFactionCore
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
    Html.map GameMsg (View.View.lazyViewScene model)



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
