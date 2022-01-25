module Components.Skill exposing (Skill, isReady, newSkill, reduceCooldown, resetCooldown)

import Components.Meter exposing (Meter, isEmpty, newMeter, setFull, subtract)


{-| Skill type. cooldown moves from cooldown time to 0. So a skill with cooldownLeft = 0 is ready to use
-}
type alias Skill t =
    { cooldown : Meter Float
    , name : String
    , description : String
    , target : Maybe t
    }


{-| create a new skill with given cooldown, name and description.

Negative cooldowns will be clamped to 0

Empty names and description will use defaults

-}
newSkill : Float -> String -> String -> Skill t
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
        Nothing


{-| Reset remaining cooldown to max cooldown
-}
resetCooldown : Skill t -> Skill t
resetCooldown skill =
    { skill | cooldown = setFull skill.cooldown }


{-| Reduce skill cooldown by amount

Negative amounts are ignored

-}
reduceCooldown : Float -> Skill t -> Skill t
reduceCooldown amount skill =
    { skill | cooldown = subtract amount skill.cooldown }


{-| Is skill ready to use predicate
-}
isReady : Skill t -> Bool
isReady skill =
    isEmpty skill.cooldown
