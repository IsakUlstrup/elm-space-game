module Systems.SkillSystem exposing (skillSystem)

import ComponentData
import Components.Skill
import Ecs
import GameData exposing (GameMsg(..), GameScene)


skillSystem : GameMsg -> GameScene -> GameScene
skillSystem msg scene =
    case msg of
        GameTick dt ->
            scene
                |> Ecs.updateComponents (ComponentData.updateSkill (Components.Skill.reduceCooldown dt))

        UseSkill skillId ->
            scene
                |> Ecs.updateComponent skillId (ComponentData.updateSkill Components.Skill.resetCooldown)

        SetSkillTarget target ->
            scene
                |> Ecs.updateComponents (ComponentData.updateSkill (Components.Skill.setTarget target))
