module Components.Skill exposing (Skill, isReady, newSkill, reduceCooldown, resetCooldown)

{-| Skill type. cooldown moves from cooldown time to 0. So a skill with cooldownLeft = 0 is ready to use
-}

import Components.Meter exposing (Meter, isEmpty, newMeter, setFull, subtract)


type alias Skill =
    { cooldown : Meter Float
    , name : String
    , description : String
    }


{-| create a new skill with given cooldown, name and description.

Negative cooldowns will be clamped to 0

Empty names and description will use defaults

-}
newSkill : Float -> String -> String -> Skill
newSkill cooldown name description =
    let
        defaultString n d =
            case n of
                "" ->
                    d

                sn ->
                    sn
    in
    Skill
        (newMeter 0 cooldown)
        (defaultString name "Unnamed Skill")
        (defaultString description "Skill description")


{-| Reset remaining cooldown to max cooldown
-}
resetCooldown : Skill -> Skill
resetCooldown skill =
    { skill | cooldown = setFull skill.cooldown }


{-| Reduce skill cooldown by amount

Negative amounts are ignored

-}
reduceCooldown : Float -> Skill -> Skill
reduceCooldown amount skill =
    { skill | cooldown = subtract amount skill.cooldown }


{-| Is skill ready to use predicate
-}
isReady : Skill -> Bool
isReady skill =
    isEmpty skill.cooldown
