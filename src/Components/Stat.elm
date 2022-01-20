module Components.Stat exposing
    ( Stat
    , StatType(..)
    , getSumHull
    , getSumPower
    , getSumShield
    , getSumStats
    , hullStat
    , powerStat
    , reduceStatValue
    , shieldStat
    , statAdd
    , statEq
    )

{-| Describes all stat types
-}


type StatType
    = Power
    | Hull
    | Shield


{-| Main Stat type, holds current stat value capped value, and stat type
-}
type alias Stat =
    { value : Int
    , cap : Int
    , statType : StatType
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

        Hull ->
            case stat2.statType of
                Hull ->
                    True

                _ ->
                    False

        Shield ->
            case stat2.statType of
                Shield ->
                    True

                _ ->
                    False


{-| create a new power stat with given value
-}
powerStat : Int -> Stat
powerStat value =
    Stat value value Power


{-| create a new hull stat with given value
-}
hullStat : Int -> Stat
hullStat value =
    Stat value value Hull


{-| create a new shield stat with given value
-}
shieldStat : Int -> Stat
shieldStat value =
    Stat value value Shield


{-| Add stat value and cap to sum if they are the same type
-}
statAdd : Stat -> Stat -> Stat
statAdd stat1 stat2 =
    case stat1.statType of
        Hull ->
            case stat2.statType of
                Hull ->
                    { stat1
                        | value = stat1.value + stat2.value
                        , cap = stat1.cap + stat2.cap
                    }

                _ ->
                    stat1

        Power ->
            case stat2.statType of
                Power ->
                    { stat1
                        | value = stat1.value + stat2.value
                        , cap = stat1.cap + stat2.cap
                    }

                _ ->
                    stat1

        Shield ->
            case stat2.statType of
                Shield ->
                    { stat1
                        | value = stat1.value + stat2.value
                        , cap = stat1.cap + stat2.cap
                    }

                _ ->
                    stat1


{-| Reduce the value of a stat by amount
-}
reduceStatValue : Int -> Stat -> Stat
reduceStatValue amount stat =
    { stat | value = clamp 0 stat.cap (stat.value - amount) }


{-| Given a target stat and a list of stats, return the sum of stats that match target
-}
getSumStat : Stat -> List Stat -> Maybe Stat
getSumStat targetStat stats =
    case List.filter (statEq targetStat) stats of
        [] ->
            Nothing

        powerStats ->
            Just (List.foldl statAdd targetStat powerStats)


{-| Given a list of stats, return the sum of all power stats if any
-}
getSumPower : List Stat -> Maybe Stat
getSumPower stats =
    getSumStat (powerStat 0) stats


{-| Given a list of stats, return the sum of all hull stats if any
-}
getSumHull : List Stat -> Maybe Stat
getSumHull stats =
    getSumStat (hullStat 0) stats


{-| Given a list of stats, return the sum of all shield stats if any
-}
getSumShield : List Stat -> Maybe Stat
getSumShield stats =
    getSumStat (shieldStat 0) stats


{-| Given a list of stats, sum them together and return a list of summed stats
-}
getSumStats : List Stat -> List Stat
getSumStats stats =
    List.filterMap identity [ getSumShield stats, getSumHull stats, getSumPower stats ]
