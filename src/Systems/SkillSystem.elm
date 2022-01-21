module Systems.SkillSystem exposing (skillSystem)

import ComponentData exposing (ComponentData)
import Components.Skill
import Ecs
import GameData exposing (GameMsg(..), GameScene)


{-| Redue skill cooldown by amount
-}
reduceSkillCooldown : Float -> ComponentData -> ComponentData
reduceSkillCooldown amount comp =
    ComponentData.updateSkill (Components.Skill.reduceCooldown amount) comp


{-| Reset skill cooldown
-}
resetSkillCooldown : ComponentData -> ComponentData
resetSkillCooldown comp =
    ComponentData.updateSkill Components.Skill.resetCooldown comp


skillSystem : GameMsg -> GameScene -> GameScene
skillSystem msg scene =
    case msg of
        GameTick dt ->
            scene
                |> Ecs.updateComponents (reduceSkillCooldown dt)

        UseSkill skillId ->
            scene
                |> Ecs.updateComponent skillId resetSkillCooldown
