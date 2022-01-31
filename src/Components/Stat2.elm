module Components.Stat2 exposing (..)

import Components.Stat exposing (Stat)


type StatModifier
    = Add Float
    | Subtract Float
    | Multiply Float
    | SetMulti Float


{-| Create add multiplier, negative values will be clamped to 0
-}
addModifier : Float -> StatModifier
addModifier i =
    Add (max 0 i)


{-| Create subtract multiplier, negative values will be clamped to 0
-}
subModifier : Float -> StatModifier
subModifier i =
    Subtract (max 0 i)


type StatType
    = Power
    | CooldownRecovery


type alias Stat =
    { statType : StatType
    , value : Float
    }


{-| Create a new power stat
-}
powerStat : Float -> Stat
powerStat init =
    Stat Power init


{-| Create a new cooldown recovery stat
-}
cooldownRecoveryStat : Float -> Stat
cooldownRecoveryStat init =
    Stat CooldownRecovery init


add : List StatModifier -> Float -> Float
add mods sum =
    let
        addAccum : StatModifier -> Float -> Float
        addAccum mod s =
            case mod of
                Add i ->
                    s + abs i

                Subtract i ->
                    s - abs i

                _ ->
                    s
    in
    List.foldl addAccum 0 mods + sum


multi : List StatModifier -> Float -> Float
multi mods sum =
    let
        multiAccum : StatModifier -> Float -> Float
        multiAccum mod s =
            case mod of
                Multiply i ->
                    s + i

                _ ->
                    s
    in
    List.foldl multiAccum 1 mods * sum


setMulti : List StatModifier -> Float -> Float
setMulti mods sum =
    let
        multiAccum : StatModifier -> Float -> Float
        multiAccum mod s =
            case mod of
                SetMulti i ->
                    s * i

                _ ->
                    s
    in
    List.foldl multiAccum 1 mods * sum


{-| Apply a list of stat modifiers, order of operations is add/subtract -> multiply -> set multiply

If a SetMultiplier of 0 is included, result will always be 0

-}
applyModifiers : List StatModifier -> Stat -> Stat
applyModifiers mods stat =
    { stat
        | value =
            stat.value
                |> add mods
                |> multi mods
                |> setMulti mods
    }
