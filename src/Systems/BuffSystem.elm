module Systems.BuffSystem exposing (buffSystem)

import ComponentData exposing (ComponentData)
import Components.Buff
import Components.Part
import Ecs
import GameData exposing (GameMsg(..), GameScene)


activeBuff : ComponentData -> Bool
activeBuff buff =
    ComponentData.buffPred Components.Buff.isActive buff


buffSystem : GameMsg -> GameScene -> GameScene
buffSystem msg scene =
    case msg of
        GameTick dt ->
            scene
                |> Ecs.updateComponents (ComponentData.updatePart (Components.Part.reduceBuffDuration dt))
                |> Ecs.filterComponents activeBuff

        _ ->
            scene
