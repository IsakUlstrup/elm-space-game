module GameData exposing (GameMsg(..), GameScene, GameSystem)

import ComponentData exposing (ComponentData)
import Components.Skill exposing (SkillEffect)
import Ecs exposing (EcsId)


type alias GameSystem =
    Ecs.System ComponentData GameMsg


type alias GameScene =
    Ecs.Scene ComponentData GameMsg


type GameMsg
    = GameTick Float
      -- (skill parent part id, skill index) (target component id, skill effect)
    | UseSkill ( EcsId, Int ) ( EcsId, SkillEffect )
    | SetSkillTarget EcsId
