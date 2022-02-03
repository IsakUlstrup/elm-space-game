module Systems.SkillSystem exposing (skillSystem)

import ComponentData
import Components.Core
import Components.Part
import Components.Skill exposing (SkillEffect(..))
import Ecs
import GameData exposing (GameMsg(..), GameScene)


skillSystem : GameMsg -> GameScene -> GameScene
skillSystem msg scene =
    case msg of
        GameTick dt ->
            scene
                |> Ecs.updateComponents (ComponentData.updatePart (Components.Part.reduceSkillCooldown dt))

        UseSkill ( partId, skillIndex ) ( targetId, skilleffect ) ->
            case skilleffect of
                Damage dmg ->
                    -- Not implemented yet
                    scene
                        |> Ecs.updateComponent partId (ComponentData.updatePart (Components.Part.resetSkillCooldown skillIndex))
                        |> Ecs.updateComponent targetId (ComponentData.updatePart (Components.Part.damage dmg))
                        |> Ecs.updateComponent targetId (ComponentData.updateCore (Components.Core.damage dmg))

                Buff buff ->
                    scene
                        |> Ecs.updateComponent partId (ComponentData.updatePart (Components.Part.resetSkillCooldown skillIndex))
                        |> Ecs.updateComponent targetId (ComponentData.updatePart (Components.Part.addBuff buff))

        SetSkillTarget target ->
            scene
                |> Ecs.updateComponents (ComponentData.updatePart (Components.Part.setSkillTarget target))
