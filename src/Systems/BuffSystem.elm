module Systems.BuffSystem exposing (buffSystem)

import ComponentData exposing (ComponentData)
import Components.Part
import Components.Stat
import Ecs
import GameData exposing (GameMsg(..), GameScene)


activeBuff : ComponentData -> Bool
activeBuff buff =
    ComponentData.buffPred Components.Stat.isActive buff


buffSystem : GameMsg -> GameScene -> GameScene
buffSystem msg scene =
    case msg of
        GameTick dt ->
            scene
                |> Ecs.updateComponents (ComponentData.updatePart (Components.Part.reduceBuffDuration dt))
                |> Ecs.filterComponents activeBuff

        _ ->
            scene
