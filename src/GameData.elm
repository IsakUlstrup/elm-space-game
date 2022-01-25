module GameData exposing (GameMsg(..), GameScene, GameSystem)

import ComponentData exposing (ComponentData)
import Ecs exposing (EcsId, Entity)


type alias GameSystem =
    Ecs.System ComponentData GameMsg


type alias GameScene =
    Ecs.Scene ComponentData GameMsg


type GameMsg
    = GameTick Float
    | UseSkill EcsId
    | SetSkillTarget Entity
