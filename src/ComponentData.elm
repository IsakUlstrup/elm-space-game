module ComponentData exposing (ComponentData, getStat, noCompData, statCompData, updateStat)

import Components.Stat exposing (Stat)


type ComponentData
    = StatData Stat
    | NoData


noCompData : ComponentData
noCompData =
    NoData



---- STAT ----


statCompData : Stat -> ComponentData
statCompData stat =
    StatData stat


{-| If given ComponentData is a stat, return stat
-}
getStat : ComponentData -> Maybe Stat
getStat cd =
    case cd of
        StatData s ->
            Just s

        _ ->
            Nothing


{-| If given ComponentData is a stat, apply f to it and return
-}
updateStat : (Stat -> Stat) -> ComponentData -> ComponentData
updateStat f cd =
    case cd of
        StatData s ->
            StatData (f s)

        _ ->
            cd
