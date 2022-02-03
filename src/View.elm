module View exposing (..)

import ComponentData
import Components.Color exposing (toCssString)
import Components.Part exposing (Part, getStats)
import Components.Range exposing (Range)
import Components.Skill exposing (Skill)
import Components.Stat exposing (Buff, Stat, StatType(..), getStatValue)
import Ecs exposing (EcsId, Entity, idToInt)
import GameData exposing (GameMsg(..), GameScene)
import Html exposing (Html, br, button, div, h3, meter, p, strong, text)
import Html.Attributes as HtmlAttr exposing (class, style)
import Html.Events


viewStat : Stat -> Html msg
viewStat stat =
    let
        statTypeString =
            case stat.statType of
                Power ->
                    "Power"

                CooldownRecovery ->
                    "Cooldown Recovery"
    in
    p []
        [ strong [] [ text statTypeString, text ": " ]
        , text (stat |> getStatValue |> String.fromFloat)
        ]


viewMeter : Range Float -> Html msg
viewMeter m =
    let
        stringAttrs =
            Components.Range.viewHelperFloat m
    in
    meter
        [ HtmlAttr.min stringAttrs.min
        , HtmlAttr.max stringAttrs.max
        , HtmlAttr.value stringAttrs.value
        ]
        []


viewSkill : EcsId -> ( Int, Skill EcsId ) -> Html GameMsg
viewSkill compId ( skillIndex, skill ) =
    let
        targetString =
            case skill.target of
                Just trgt ->
                    idToInt trgt |> String.fromInt

                Nothing ->
                    "None"
    in
    let
        attrs =
            case skill.target of
                Just trgt ->
                    -- ( EcsId, Int ) ( EcsId, SkillEffect )
                    [ Html.Events.onClick (UseSkill ( compId, skillIndex ) ( trgt, skill.effect ))
                    , HtmlAttr.disabled (Components.Skill.isReady skill |> not)
                    , class "skill"
                    ]

                Nothing ->
                    [ HtmlAttr.disabled True
                    , class "skill"
                    ]
    in
    button
        attrs
        [ h3 [] [ text (String.fromInt skillIndex), text " ", text skill.name ]
        , p [] [ text skill.description ]
        , p [] [ text "id: ", text (String.fromInt (Ecs.idToInt compId)) ]
        , p [] [ text "target: ", text targetString ]
        , viewMeter skill.cooldown
        ]


viewBuff : Buff -> Html GameMsg
viewBuff buff =
    let
        viewDuration b =
            case b.duration of
                Just d ->
                    viewMeter d

                Nothing ->
                    p [] [ text "Infinite" ]
    in
    p [ class "buff" ]
        [ text buff.description
        , br [] []
        , text "duration: "
        , viewDuration buff
        ]


viewPart : EcsId -> Part EcsId -> Html GameMsg
viewPart pid part =
    div [ class "part" ]
        [ h3
            [ style "color" (toCssString part.color)
            , Html.Events.onClick (SetSkillTarget pid)
            ]
            [ text (idToInt pid |> String.fromInt), text " Module: ", text part.name ]
        , p [] [ text "durability: ", viewMeter part.durability ]
        , div [] (h3 [] [ text "skills" ] :: List.indexedMap (\i s -> viewSkill pid ( i, s )) part.skills)
        , div [] (h3 [] [ text "buffs" ] :: List.map viewBuff part.stats)
        , div [] (h3 [] [ text "stats" ] :: List.map viewStat (getStats part))
        ]


viewEntity2 : EcsId -> ComponentData.ComponentData -> Maybe (Html GameMsg)
viewEntity2 cid component =
    ComponentData.getPart component |> Maybe.andThen (\p -> Just (viewPart cid p))


viewEntity3 : ( Entity, List ( EcsId, ComponentData.ComponentData ) ) -> Html GameMsg
viewEntity3 ( entity, components ) =
    div [ class "entity" ]
        [ h3 [] [ text "entity" ]
        , div [ style "display" "flex" ] (List.filterMap (\( cid, cdata ) -> viewEntity2 cid cdata) components)
        ]


viewScene : GameScene -> Html GameMsg
viewScene scene =
    div [ style "display" "flex" ] (Ecs.mapComponentGroups viewEntity3 scene)
