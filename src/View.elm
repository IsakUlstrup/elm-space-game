module View exposing (..)

import ComponentData
import Components.Color
import Components.Meter exposing (Meter, isEmpty)
import Components.Skill exposing (Skill)
import Components.Stat exposing (Stat, StatType(..))
import Ecs exposing (EcsId, Entity)
import GameData exposing (GameMsg(..), GameScene)
import Html exposing (Html, button, div, h2, h3, meter, p, text)
import Html.Attributes as HtmlAttr exposing (class)
import Html.Events


viewStat : Stat -> List (Html msg)
viewStat stat =
    let
        statTypeString =
            case stat.statType of
                Power ->
                    "Power"

                Hull ->
                    "Hull"

                Shield ->
                    "Shield"
    in
    [ h3 [] [ text statTypeString ]
    , p []
        [ text (String.fromInt stat.value)
        , text "/"
        , text (String.fromInt stat.cap)
        ]
    ]


viewMeter : Meter Float -> Html msg
viewMeter m =
    let
        stringAttrs =
            Components.Meter.viewHelperFloat m
    in
    meter
        [ HtmlAttr.min stringAttrs.min
        , HtmlAttr.max stringAttrs.max
        , HtmlAttr.value stringAttrs.value
        ]
        []


viewSkill : ( EcsId, Skill ) -> List (Html GameMsg)
viewSkill ( compId, skill ) =
    [ h3 [] [ text skill.name ]
    , p [] [ text skill.description ]
    , p [] [ text "id: ", text (String.fromInt (Ecs.idToInt compId)) ]
    , viewMeter skill.cooldown
    , button [ Html.Events.onClick (UseSkill compId), HtmlAttr.disabled (isEmpty skill.cooldown |> not) ] [ text "Use skill" ]
    ]


viewEntity : ( Entity, List ( EcsId, ComponentData.ComponentData ) ) -> Maybe (Html GameMsg)
viewEntity ( _, components ) =
    let
        color =
            List.filterMap ComponentData.getColor (List.map Tuple.second components)
                |> List.head
                |> Maybe.withDefault (Components.Color.initColor |> Components.Color.withLightness 0)
                |> Components.Color.toCssString
    in
    case
        ( List.filterMap ComponentData.getStat (List.map Tuple.second components)
        , List.filterMap (\( cid, comp ) -> ComponentData.getSkill comp |> Maybe.andThen (\s -> Just ( cid, s ))) components
        )
    of
        ( [], [] ) ->
            Nothing

        ( stats, [] ) ->
            Components.Stat.getSumStats stats
                |> Maybe.andThen (\ss -> Just (div [ class "entity" ] (h2 [ HtmlAttr.style "color" color ] [ text "Entity" ] :: List.concatMap viewStat ss)))

        ( [], skills ) ->
            Just (div [ class "entity" ] (h2 [ HtmlAttr.style "color" color ] [ text "Entity" ] :: List.concatMap viewSkill skills))

        ( stats, skills ) ->
            Components.Stat.getSumStats stats
                |> Maybe.andThen
                    (\ss ->
                        Just (div [ class "entity" ] (h2 [ HtmlAttr.style "color" color ] [ text "Entity" ] :: List.concatMap viewStat ss ++ List.concatMap viewSkill skills))
                    )


viewScene : GameScene -> Html GameMsg
viewScene scene =
    -- div [] [ viewPlayers (Ecs.getEntitiesWithComponents scene) ]
    div [] (List.filterMap identity (Ecs.mapEntitiesWithComponents viewEntity scene))
