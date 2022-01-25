module ComponentData exposing
    ( ComponentData
    , colorCompData
    , getColor
    , getSkill
    , getStat
    , skillCompData
    , statCompData
    , updateColor
    , updateSkill
    , updateStat
    )

import Components.Color exposing (Color)
import Components.Skill exposing (Skill)
import Components.Stat exposing (Stat)


type ComponentData
    = StatData Stat
    | SkillData Skill
    | ColorData Color



---- STAT ----


{-| Contruct new StatData with provided stat
-}
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



---- SKILL ----


{-| Contruct new SkillData with provided skill
-}
skillCompData : Skill -> ComponentData
skillCompData skill =
    SkillData skill


{-| If given ComponentData is a skill, return skill
-}
getSkill : ComponentData -> Maybe Skill
getSkill cd =
    case cd of
        SkillData s ->
            Just s

        _ ->
            Nothing


{-| If given ComponentData is a skill, apply f to it and return
-}
updateSkill : (Skill -> Skill) -> ComponentData -> ComponentData
updateSkill f cd =
    case cd of
        SkillData s ->
            SkillData (f s)

        _ ->
            cd



---- COLOR ----


{-| Contruct new ColorData with provided color
-}
colorCompData : Color -> ComponentData
colorCompData color =
    ColorData color


{-| If given ComponentData is a color, return color
-}
getColor : ComponentData -> Maybe Color
getColor cd =
    case cd of
        ColorData c ->
            Just c

        _ ->
            Nothing


{-| If given ComponentData is a color, apply f to it and return
-}
updateColor : (Color -> Color) -> ComponentData -> ComponentData
updateColor f cd =
    case cd of
        ColorData c ->
            ColorData (f c)

        _ ->
            cd
