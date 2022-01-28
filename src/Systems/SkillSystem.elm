module Systems.SkillSystem exposing (skillSystem)

import ComponentData
import Components.Skill exposing (SkillEffect(..))
import Ecs
import GameData exposing (GameMsg(..), GameScene)


skillSystem : GameMsg -> GameScene -> GameScene
skillSystem msg scene =
    case msg of
        GameTick dt ->
            scene
                |> Ecs.updateComponents (ComponentData.updateSkill (Components.Skill.reduceCooldown dt))

        UseSkill skillId target effect ->
            case effect of
                Damage _ ->
                    -- Not implemented yet
                    scene
                        |> Ecs.updateComponent skillId (ComponentData.updateSkill Components.Skill.resetCooldown)

                Buff buff ->
                    scene
                        |> Ecs.updateComponent skillId (ComponentData.updateSkill Components.Skill.resetCooldown)
                        |> Ecs.addComponents target [ ComponentData.buffCompData buff ]

        SetSkillTarget target ->
            scene
                |> Ecs.updateComponents (ComponentData.updateSkill (Components.Skill.setTarget target))
