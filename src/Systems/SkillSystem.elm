module Systems.SkillSystem exposing (skillSystem)

import ComponentData exposing (updateSkill)
import Components.Skill
import Ecs exposing (updateComponent, updateComponents)
import GameData exposing (GameMsg(..), GameScene)


skillSystem : GameMsg -> GameScene -> GameScene
skillSystem msg scene =
    case msg of
        GameTick dt ->
            scene |> updateComponents (updateSkill (Components.Skill.reduceCooldown dt))

        UseSkill skillId ->
            scene |> updateComponent skillId (updateSkill Components.Skill.resetCooldown)
