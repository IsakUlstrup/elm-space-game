module View exposing (..)

import ComponentData
import Ecs exposing (Entity)
import GameData exposing (GameScene)
import Html exposing (Html, br, div, text)
import Html.Attributes exposing (class)



-- viewPlayer : Bool -> List Module -> Html msg
-- viewPlayer _ modules =
--     div [ class "player" ]
--         [ text "player"
--         , br [] []
--         , text "power: "
--         , text (ComponentData.getPower modules |> String.fromInt)
--         , br [] []
--         , text "hull: "
--         , text (ComponentData.getHull modules |> String.fromInt)
--         ]


viewPlayers : List ( Entity, List ComponentData.ComponentData ) -> Html msg
viewPlayers entities =
    -- div [] (List.filterMap (\( _, comps ) -> Maybe.map2 viewPlayer (ComponentData.getControllable comps) (ComponentData.getModules comps)) entities)
    div [] []


viewScene : GameScene -> Html msg
viewScene scene =
    div [] [ viewPlayers (Ecs.getEntitiesWithComponents scene) ]
