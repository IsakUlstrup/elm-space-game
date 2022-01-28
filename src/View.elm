module View exposing (..)

import ComponentData
import Components.Buff exposing (Buff)
import Components.Color
import Components.Meter exposing (Meter)
import Components.Skill exposing (Skill)
import Components.Stat exposing (Stat, StatType(..))
import Ecs exposing (EcsId, Entity)
import GameData exposing (GameMsg(..), GameScene)
import Html exposing (Html, br, button, div, h2, h3, meter, p, strong, text)
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
    [ p []
        [ strong [] [ text statTypeString, text ": " ]
        , text (String.fromInt stat.value)
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


viewSkill : ( EcsId, Skill Ecs.Entity ) -> List (Html GameMsg)
viewSkill ( compId, skill ) =
    let
        attrs =
            case skill.target of
                Just trgt ->
                    [ Html.Events.onClick (UseSkill compId trgt skill.effect)
                    , HtmlAttr.disabled (Components.Skill.isReady skill |> not)
                    ]

                Nothing ->
                    [ HtmlAttr.disabled True
                    ]
    in
    [ button
        attrs
        [ h3 [] [ text skill.name ]
        , p [] [ text skill.description ]
        , p [] [ text "id: ", text (String.fromInt (Ecs.idToInt compId)) ]
        , p [] [ text (Debug.toString skill.target) ]
        , viewMeter skill.cooldown
        ]
    ]


viewBuff : Buff -> List (Html GameMsg)
viewBuff buff =
    let
        viewDuration b =
            case b.duration of
                Just d ->
                    viewMeter d

                Nothing ->
                    p [] [ text "Infinite" ]
    in
    [ p [ class "buff" ]
        [ text buff.description
        , br [] []
        , text "duration: "
        , viewDuration buff
        ]

    -- , p [] [ text "effects:" ]
    -- , ul [] (List.map (\s -> li [] (viewStat s)) buff.effects)
    -- , p [] [ text "duration: ", viewDuration buff ]
    ]


viewEntity : ( Entity, List ( EcsId, ComponentData.ComponentData ) ) -> Maybe (Html GameMsg)
viewEntity ( entity, components ) =
    let
        color =
            List.filterMap ComponentData.getColor (List.map Tuple.second components)
                |> List.head
                |> Maybe.withDefault (Components.Color.initColor |> Components.Color.withLightness 0)
                |> Components.Color.toCssString

        clickTargetEvent e =
            Html.Events.onClick (SetSkillTarget e)

        entityWrapper e cs =
            div [ class "entity" ]
                (h2 [ HtmlAttr.style "color" color, clickTargetEvent e ] [ text (Debug.toString e) ] :: cs)
    in
    case
        ( List.filterMap ComponentData.getStat (List.map Tuple.second components)
        , List.filterMap (\( cid, comp ) -> ComponentData.getSkill comp |> Maybe.andThen (\s -> Just ( cid, s ))) components
        , List.filterMap ComponentData.getBuff (List.map Tuple.second components)
        )
    of
        ( [], [], [] ) ->
            Nothing

        ( stats, skills, buffs ) ->
            Just
                (entityWrapper entity
                    (List.concatMap viewSkill skills
                        ++ h3 [] [ text "Stats" ]
                        :: (Components.Stat.getSumStats (stats ++ List.concatMap (\b -> b.effects) buffs)
                                |> Maybe.andThen (\ss -> Just (List.concatMap viewStat ss))
                                |> Maybe.withDefault []
                           )
                        ++ List.concatMap viewBuff buffs
                    )
                )


viewScene : GameScene -> Html GameMsg
viewScene scene =
    div [ class "entities" ] (List.filterMap identity (Ecs.mapEntitiesWithComponents viewEntity scene))
