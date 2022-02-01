module ComponentData exposing
    ( ComponentData
    , buffCompData
    , buffPred
    , colorCompData
    , getBuff
    , getColor
    , getPart
    , getSkill
    , getStat
    , partCompData
    , partPred
    , skillCompData
    , statCompData
    , updateBuff
    , updateColor
    , updatePart
    , updateSkill
    , updateStat
    )

import Components.Buff exposing (Buff)
import Components.Color exposing (Color)
import Components.Part exposing (Part)
import Components.Skill exposing (Skill)
import Components.Stat exposing (Stat)
import Ecs exposing (EcsId)


type ComponentData
    = StatData Stat
    | SkillData (Skill Ecs.Entity)
    | ColorData Color
    | BuffData Buff
    | PartData (Part Ecs.EcsId)



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
skillCompData : Skill Ecs.Entity -> ComponentData
skillCompData skill =
    SkillData skill


{-| If given ComponentData is a skill, return skill
-}
getSkill : ComponentData -> Maybe (Skill Ecs.Entity)
getSkill cd =
    case cd of
        SkillData s ->
            Just s

        _ ->
            Nothing


{-| If given ComponentData is a skill, apply f to it and return
-}
updateSkill : (Skill Ecs.Entity -> Skill Ecs.Entity) -> ComponentData -> ComponentData
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



---- BUFF ----


{-| Contruct new BuffData with provided buff
-}
buffCompData : Buff -> ComponentData
buffCompData buff =
    BuffData buff


{-| If given ComponentData is a buff, return buff
-}
getBuff : ComponentData -> Maybe Buff
getBuff cd =
    case cd of
        BuffData b ->
            Just b

        _ ->
            Nothing


{-| If given ComponentData is a buff, apply f to it and return
-}
updateBuff : (Buff -> Buff) -> ComponentData -> ComponentData
updateBuff f cd =
    case cd of
        BuffData b ->
            BuffData (f b)

        _ ->
            cd


{-| Buff predicate
-}
buffPred : (Buff -> Bool) -> ComponentData -> Bool
buffPred f cd =
    case cd of
        BuffData b ->
            f b

        _ ->
            True



---- PART ----


{-| Contruct new PartData with provided part
-}
partCompData : Part Ecs.EcsId -> ComponentData
partCompData part =
    PartData part


{-| If given ComponentData is a part, return part
-}
getPart : ComponentData -> Maybe (Part Ecs.EcsId)
getPart cd =
    case cd of
        PartData s ->
            Just s

        _ ->
            Nothing


{-| If given ComponentData is a part, apply f to it and return
-}
updatePart : (Part Ecs.EcsId -> Part Ecs.EcsId) -> ComponentData -> ComponentData
updatePart f cd =
    case cd of
        PartData s ->
            PartData (f s)

        _ ->
            cd


{-| Part predicate
-}
partPred : (Part EcsId -> Bool) -> ComponentData -> Bool
partPred f cd =
    case cd of
        PartData b ->
            f b

        _ ->
            True
