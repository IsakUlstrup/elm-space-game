module View.View exposing (..)

import ComponentData
import Components.Color exposing (toCssString)
import Components.Core exposing (Core, Faction(..))
import Components.Part exposing (Part, getStats)
import Components.Range exposing (Range)
import Components.Skill exposing (Skill)
import Components.Stat exposing (Buff, Stat, StatType(..), getStatValue)
import Ecs exposing (EcsId, Entity, idToInt)
import GameData exposing (GameMsg(..), GameScene)
import Html exposing (Html, br, button, div, h3, meter, p, strong, text)
import Html.Attributes as HtmlAttr exposing (class, style)
import Html.Events
import Html.Lazy


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


viewSkill : Bool -> EcsId -> ( Int, Skill EcsId ) -> Html GameMsg
viewSkill player compId ( skillIndex, skill ) =
    let
        targetString =
            case skill.target of
                Just trgt ->
                    idToInt trgt |> String.fromInt

                Nothing ->
                    "None"
    in
    let
        attrs : List (Html.Attribute GameMsg)
        attrs =
            if player then
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

            else
                [ class "skill" ]

        container : List (Html GameMsg) -> Html GameMsg
        container cs =
            if player then
                button attrs cs

            else
                div attrs cs
    in
    container
        [ h3 [] [ text (String.fromInt skillIndex), text " ", text skill.name ]
        , p [] [ text skill.description ]
        , p [] [ text "id: ", text (String.fromInt (Ecs.idToInt compId)) ]
        , p [] [ text "target: ", text targetString ]
        , viewMeter skill.cooldown
        ]


viewBuff : Buff -> Html GameMsg
viewBuff buff =
    let
        viewDuration : Buff -> Html msg
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


viewPart : Bool -> ( EcsId, Part EcsId ) -> Html GameMsg
viewPart player ( pid, part ) =
    div [ class "part" ]
        [ h3
            [ style "color" (toCssString part.color)
            , Html.Events.onClick (SetSkillTarget pid)
            ]
            [ text (idToInt pid |> String.fromInt), text " Module: ", text part.name ]
        , p [] [ text "durability: ", viewMeter part.durability ]
        , div [] (h3 [] [ text "skills" ] :: List.indexedMap (\i s -> viewSkill player pid ( i, s )) part.skills)
        , div [] (h3 [] [ text "buffs" ] :: List.map viewBuff part.stats)
        , div [] (h3 [] [ text "stats" ] :: List.map viewStat (getStats part))
        ]


viewCore : ( EcsId, Core ) -> Html GameMsg
viewCore ( cid, core ) =
    let
        factionString c =
            case c of
                Player _ ->
                    "Player"

                Faction1 ->
                    "Faction1"

                Faction2 ->
                    "Faction2"

                Neutral ->
                    "Neutral"
    in
    div []
        [ h3 [ Html.Events.onClick (SetSkillTarget cid) ] [ text "core: ", text (factionString core.faction) ]
        , p [] [ text "durability: ", viewMeter core.durability ]
        ]


type alias RenderEntity =
    { id : Entity
    , parts : List ( EcsId, Part EcsId )
    , cores : List ( EcsId, Core )
    }


formatEntity : ( Entity, List ( EcsId, ComponentData.ComponentData ) ) -> Maybe RenderEntity
formatEntity ( entity, components ) =
    let
        nonEmptyList l =
            case l of
                [] ->
                    Nothing

                ls ->
                    Just ls
    in
    Maybe.map2 (RenderEntity entity)
        (List.filterMap
            (\( id, c ) ->
                ComponentData.getPart c |> Maybe.andThen (\p -> Just ( id, p ))
            )
            components
            |> nonEmptyList
        )
        (List.filterMap
            (\( id, c ) ->
                ComponentData.getCore c |> Maybe.andThen (\p -> Just ( id, p ))
            )
            components
            |> nonEmptyList
        )


viewPlayer : RenderEntity -> Html GameMsg
viewPlayer player =
    div [ class "player entity" ]
        [ h3 [] [ text "player" ]
        , div [] (List.map viewCore player.cores)
        , div [] (List.map (viewPart True) player.parts)
        ]


viewEntity : RenderEntity -> Html GameMsg
viewEntity entity =
    div [ class "entity" ]
        [ h3 [] [ text "non player entity" ]
        , div [] (List.map viewCore entity.cores)
        , div [] (List.map (viewPart False) entity.parts)
        ]


viewScene : GameScene -> Html GameMsg
viewScene scene =
    let
        renderEntities s =
            Ecs.mapComponentGroups formatEntity s
                |> List.filterMap identity

        noIdCore ( i, c ) =
            c

        players : List RenderEntity -> List RenderEntity
        players es =
            List.filter (\e -> List.filter Components.Core.isPlayer (List.map noIdCore e.cores) |> List.isEmpty |> not) es

        nonPlayers : List RenderEntity -> List RenderEntity
        nonPlayers es =
            List.filter (\e -> List.filter Components.Core.isPlayer (List.map noIdCore e.cores) |> List.isEmpty) es
    in
    div [ style "display" "flex" ]
        [ div [ style "display" "flex" ] (List.map viewPlayer (players (renderEntities scene)))
        , div [ style "display" "flex" ] (List.map viewEntity (nonPlayers (renderEntities scene)))
        ]


lazyViewScene : GameScene -> Html GameMsg
lazyViewScene scene =
    Html.Lazy.lazy viewScene scene
