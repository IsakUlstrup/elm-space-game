module View exposing (..)

import ComponentData
import Components.Stat exposing (Stat, StatType(..))
import Ecs exposing (Entity)
import GameData exposing (GameScene)
import Html exposing (Html, div, h2, h3, p, text)
import Html.Attributes exposing (class)


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


viewEntity : ( Entity, List ComponentData.ComponentData ) -> Maybe (Html msg)
viewEntity ( entity, components ) =
    case List.filterMap ComponentData.getStat components of
        [] ->
            Nothing

        stats ->
            case Components.Stat.getSumStats stats of
                [] ->
                    Nothing

                s ->
                    Just (div [ class "entity" ] (h2 [] [ text "Entity" ] :: List.concatMap viewStat s))


viewScene : GameScene -> Html msg
viewScene scene =
    -- div [] [ viewPlayers (Ecs.getEntitiesWithComponents scene) ]
    div [] (List.filterMap viewEntity (Ecs.getEntitiesWithComponents scene))
