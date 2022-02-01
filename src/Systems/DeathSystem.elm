module Systems.DeathSystem exposing (deathSystem)

import ComponentData exposing (ComponentData)
import Components.Part
import Ecs
import GameData exposing (GameMsg(..), GameScene)


activePart : ComponentData -> Bool
activePart part =
    ComponentData.partPred Components.Part.isAlive part


deathSystem : GameMsg -> GameScene -> GameScene
deathSystem msg scene =
    case msg of
        GameTick _ ->
            scene
                |> Ecs.filterComponents activePart

        _ ->
            scene
