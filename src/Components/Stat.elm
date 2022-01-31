module Components.Stat exposing (..)


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


setSubModifier : Float -> StatModifier
setSubModifier i =
    SetMulti (max 0 i)


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


{-| Stat equality, useful for filtering similar stats
-}
statEq : Stat -> Stat -> Bool
statEq stat1 stat2 =
    case stat1.statType of
        Power ->
            case stat2.statType of
                Power ->
                    True

                _ ->
                    False

        CooldownRecovery ->
            case stat2.statType of
                CooldownRecovery ->
                    True

                _ ->
                    False


{-| Add stat value and cap to sum if they are the same type
-}
statAdd : Stat -> Stat -> Stat
statAdd stat1 stat2 =
    case stat1.statType of
        Power ->
            case stat2.statType of
                Power ->
                    { stat1
                        | value = stat1.value + stat2.value
                    }

                _ ->
                    stat1

        CooldownRecovery ->
            case stat2.statType of
                CooldownRecovery ->
                    { stat1
                        | value = stat1.value + stat2.value
                    }

                _ ->
                    stat1


{-| Given a target stat and a list of stats, return the sum of stats that match target
-}
getSumStat : Stat -> List Stat -> Maybe Stat
getSumStat targetStat stats =
    case List.filter (statEq targetStat) stats of
        [] ->
            Nothing

        matchingStats ->
            Just (List.foldl statAdd targetStat matchingStats)


{-| Given a list of stats, return the sum of all power stats if any
-}
getSumPower : List Stat -> Maybe Stat
getSumPower stats =
    getSumStat (powerStat 0) stats


{-| Given a list of stats, return the sum of all cooldown recovery stats if any
-}
getSumCooldownRecovery : List Stat -> Maybe Stat
getSumCooldownRecovery stats =
    getSumStat (cooldownRecoveryStat 0) stats


getSumStats : List Stat -> Maybe (List Stat)
getSumStats stats =
    case List.filterMap identity [ getSumPower stats, getSumCooldownRecovery stats ] of
        [] ->
            Nothing

        sumStats ->
            Just sumStats
