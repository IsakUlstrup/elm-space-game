module Systems.BuffSystem exposing (buffSystem)

import ComponentData exposing (ComponentData)
import Components.Buff
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
                |> Ecs.updateComponents (ComponentData.updateBuff (Components.Buff.reduceDuration dt))
                |> Ecs.filterComponents activeBuff

        _ ->
            scene
