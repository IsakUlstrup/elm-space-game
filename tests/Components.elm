module Components exposing (..)

import ComponentData exposing (getSkill, getStat, skillCompData, statCompData, updateSkill)
import Components.Buff exposing (Buff)
import Components.Color
import Components.Meter exposing (newMeter)
import Components.Skill exposing (Skill, SkillEffect(..), buffEffect, damageEffect, newSkill, reduceCooldown, resetCooldown)
import Components.Stat exposing (StatModifier(..), StatType(..), powerStat)
import Expect
import Fuzz exposing (..)
import Test exposing (..)


compData : Test
compData =
    describe "ComponentData tests"
        [ test "Get stat from ComponentData thats's a StatData variant, should return stat" <|
            \_ ->
                getStat (statCompData (powerStat 0))
                    |> Expect.equal
                        (Just
                            { modifiers = [ Add 0 ]
                            , statType = Power
                            }
                        )
        , test "Get skill from ComponentData thats's a SkillData variant, should return skill" <|
            \_ ->
                getSkill (skillCompData (newSkill 0 "" "" (Components.Skill.damageEffect 10)))
                    |> Expect.equal
                        (Just
                            { cooldown = newMeter 0 0
                            , name = "Unnamed Skill"
                            , description = "Skill description"
                            , target = Nothing
                            , effect = Damage 10
                            }
                        )
        , test "Get stat from ComponentData thats's a SkillData variant, should return nothing" <|
            \_ ->
                getStat (skillCompData (newSkill 0 "" "" (damageEffect 10)))
                    |> Expect.equal
                        Nothing
        , test "Update SkillData, should return SkillData skill with cooldown 0" <|
            \_ ->
                updateSkill resetCooldown (skillCompData (newSkill 100 "" "" (damageEffect 10)))
                    |> Expect.equal
                        (skillCompData
                            { cooldown = newMeter 100 100
                            , name = "Unnamed Skill"
                            , description = "Skill description"
                            , target = Nothing
                            , effect = Damage 10
                            }
                        )
        , test "Attempt SkillData update on Statdata, should return unchanged StatData" <|
            \_ ->
                updateSkill resetCooldown
                    (statCompData
                        { modifiers = []
                        , statType = Power
                        }
                    )
                    |> Expect.equal
                        (statCompData
                            { modifiers = []
                            , statType = Power
                            }
                        )
        ]



-- stat : Test
-- stat =
--     describe "Stat component tests"
--         [ test "Create a new hull stat with a value of 5" <|
--             \_ ->
--                 hullStat 5
--                     |> Expect.equal
--                         { value = 5
--                         , cap = 5
--                         , statType = Hull
--                         }
--         , test "Create a new power stat with a value of 5" <|
--             \_ ->
--                 powerStat 5
--                     |> Expect.equal
--                         { value = 5
--                         , cap = 5
--                         , statType = Power
--                         }
--         , test "Create a new shield stat with a value of 5" <|
--             \_ ->
--                 shieldStat 5
--                     |> Expect.equal
--                         { value = 5
--                         , cap = 5
--                         , statType = Shield
--                         }
--         , test "Add two stats with equal type together, should return the sum of their values" <|
--             \_ ->
--                 statAdd
--                     (shieldStat 5)
--                     (shieldStat 4)
--                     |> Expect.equal
--                         { value = 9
--                         , cap = 9
--                         , statType = Shield
--                         }
--         , test "Add two stats with non equal type together, should return the first stat" <|
--             \_ ->
--                 statAdd
--                     (shieldStat 5)
--                     (powerStat 4)
--                     |> Expect.equal
--                         { value = 5
--                         , cap = 5
--                         , statType = Shield
--                         }
--         , test "Compare two stats of same type for equality" <|
--             \_ ->
--                 statEq
--                     (shieldStat 5)
--                     (shieldStat 1)
--                     |> Expect.equal
--                         True
--         , test "Compare two stats of different type for equality" <|
--             \_ ->
--                 statEq
--                     (shieldStat 5)
--                     (powerStat 1)
--                     |> Expect.equal
--                         False
--         , test "Get sum of power stats in a list of varius stats" <|
--             \_ ->
--                 Components.Stat.getSumPower
--                     [ powerStat 3, shieldStat 5, hullStat 1 ]
--                     |> Expect.equal
--                         (Just
--                             { value = 3
--                             , cap = 3
--                             , statType = Power
--                             }
--                         )
--         , test "Get sum of power stats in an empty list" <|
--             \_ ->
--                 Components.Stat.getSumPower
--                     []
--                     |> Expect.equal
--                         Nothing
--         , test "Get sum of power stats in a list without any power stats" <|
--             \_ ->
--                 Components.Stat.getSumPower
--                     [ shieldStat 5, hullStat 1 ]
--                     |> Expect.equal
--                         Nothing
--         , test "Get sum of all stats present in a list of all stats" <|
--             \_ ->
--                 getSumStats
--                     [ shieldStat 5, hullStat 1, hullStat 1, powerStat 5 ]
--                     |> Expect.equal
--                         (Just
--                             [ { value = 5
--                               , cap = 5
--                               , statType = Shield
--                               }
--                             , { value = 2
--                               , cap = 2
--                               , statType = Hull
--                               }
--                             , { value = 5
--                               , cap = 5
--                               , statType = Power
--                               }
--                             ]
--                         )
--         , test "Get sum of all stats present in a list of some, but not all stats" <|
--             \_ ->
--                 getSumStats
--                     [ hullStat 1, hullStat 1, powerStat 5, hullStat 5 ]
--                     |> Expect.equal
--                         (Just
--                             [ { value = 7
--                               , cap = 7
--                               , statType = Hull
--                               }
--                             , { value = 5
--                               , cap = 5
--                               , statType = Power
--                               }
--                             ]
--                         )
--         , test "Get sum of all stats present in an empty list, should be Nothing" <|
--             \_ ->
--                 getSumStats
--                     []
--                     |> Expect.equal
--                         Nothing
--         , test "Reduce the value of a 5 value power stat by 2" <|
--             \_ ->
--                 reduceStatValue 2 (powerStat 5)
--                     |> Expect.equal
--                         { value = 3
--                         , cap = 5
--                         , statType = Power
--                         }
--         , test "Reduce the value of a 5 value power stat by 7" <|
--             \_ ->
--                 reduceStatValue 7 (powerStat 5)
--                     |> Expect.equal
--                         { value = 0
--                         , cap = 5
--                         , statType = Power
--                         }
--         , test "Reduce the value of a 5 value power stat by -7" <|
--             \_ ->
--                 reduceStatValue -7 (powerStat 5)
--                     |> Expect.equal
--                         { value = 5
--                         , cap = 5
--                         , statType = Power
--                         }
--         ]


testSkill : Skill Int
testSkill =
    { cooldown = newMeter 0 1000
    , name = "Skill"
    , description = "A skill"
    , target = Nothing
    , effect = Damage 10
    }


skillBuff : Buff
skillBuff =
    Components.Buff.newBuff "Skill buff" "Skill effect buff" [ Components.Stat.powerStat 3 ] (Just (newMeter 1000 1000))


skill : Test
skill =
    describe "Skill tests"
        [ test "Create a new skill with positive cooldown" <|
            \_ ->
                newSkill 1000 "Skill" "A skill" (damageEffect 10)
                    |> Expect.equal
                        { cooldown = newMeter 1000 1000
                        , name = "Skill"
                        , description = "A skill"
                        , target = Nothing
                        , effect = Damage 10
                        }
        , test "Create a new skill with buff skill effect" <|
            \_ ->
                newSkill 1000 "Skill" "A skill" (buffEffect skillBuff)
                    |> Expect.equal
                        { cooldown = newMeter 1000 1000
                        , name = "Skill"
                        , description = "A skill"
                        , target = Nothing
                        , effect =
                            Components.Skill.Buff
                                { name = "Skill buff"
                                , description = "Skill effect buff"
                                , effects = [ Components.Stat.powerStat 3 ]
                                , duration = Just (newMeter 1000 1000)
                                }
                        }
        , test "Create a new skill with invalid negative cooldown, should return a skill with 0 cooldown" <|
            \_ ->
                newSkill -1000 "Skill" "A skill" (damageEffect 10)
                    |> Expect.equal
                        { cooldown = newMeter 0 0
                        , name = "Skill"
                        , description = "A skill"
                        , target = Nothing
                        , effect = Damage 10
                        }
        , test "Create a new skill with empty name, should return skill with default name" <|
            \_ ->
                newSkill 1000 "" "A skill" (damageEffect 10)
                    |> Expect.equal
                        { cooldown = newMeter 1000 1000
                        , name = "Unnamed Skill"
                        , description = "A skill"
                        , target = Nothing
                        , effect = Damage 10
                        }
        , test "Create a new skill with empty description, shoild return a skill with default description" <|
            \_ ->
                newSkill 1000 "Skill" "" (damageEffect 10)
                    |> Expect.equal
                        { cooldown = newMeter 1000 1000
                        , name = "Skill"
                        , description = "Skill description"
                        , target = Nothing
                        , effect = Damage 10
                        }
        , test "Reset skill cooldown to max" <|
            \_ ->
                resetCooldown (newSkill 1000 "Skill" "A skill" (damageEffect 10))
                    |> Expect.equal
                        { cooldown = newMeter 1000 1000
                        , name = "Skill"
                        , description = "A skill"
                        , target = Nothing
                        , effect = Damage 10
                        }
        , test "Reduce skill cooldown on a skill at max cooldown by 100" <|
            \_ ->
                reduceCooldown 100 (newSkill 1000 "Skill" "A skill" (damageEffect 10) |> resetCooldown)
                    |> Expect.equal
                        { cooldown = newMeter 900 1000
                        , name = "Skill"
                        , description = "A skill"
                        , target = Nothing
                        , effect = Damage 10
                        }
        , test "Reduce skill cooldown on a skill at max cooldown by a negative number -100, should default to 0" <|
            \_ ->
                reduceCooldown -100 (newSkill 1000 "Skill" "A skill" (damageEffect 10) |> resetCooldown)
                    |> Expect.equal
                        { cooldown = newMeter 1000 1000
                        , name = "Skill"
                        , description = "A skill"
                        , target = Nothing
                        , effect = Damage 10
                        }
        , test "Reduce skill cooldown on a skill where remaining cooldown is greater than cooldown time, should clamp cooldownLeft within 0 to cooldownTime " <|
            \_ ->
                reduceCooldown 100
                    { cooldown = newMeter 2000 1000
                    , name = "Skill"
                    , description = "A skill"
                    , target = Nothing
                    , effect = Damage 10
                    }
                    |> Expect.equal
                        { cooldown = newMeter 900 1000
                        , name = "Skill"
                        , description = "A skill"
                        , target = Nothing
                        , effect = Damage 10
                        }
        , test "Reduce skill cooldown on a skill by a huge number that would put cooldown at a negative number, cooldown should be clamped to minumun 0" <|
            \_ ->
                reduceCooldown 10000 (newSkill 1000 "Skill" "A skill" (damageEffect 10) |> resetCooldown)
                    |> Expect.equal
                        { cooldown = newMeter 0 1000
                        , name = "Skill"
                        , description = "A skill"
                        , target = Nothing
                        , effect = Damage 10
                        }
        , test "Check if a skill with cooldown remaining = 0 and target set is ready to use, should be true" <|
            \_ ->
                Components.Skill.isReady (reduceCooldown 10000 (testSkill |> Components.Skill.setTarget 1))
                    |> Expect.equal
                        True
        , test "Check if a skill with cooldown remaining > 0 is ready to use, should be false" <|
            \_ ->
                Components.Skill.isReady (reduceCooldown 100 (newSkill 1000 "Skill" "A skill" (damageEffect 10) |> resetCooldown))
                    |> Expect.equal
                        False
        , test "Check if a skill with negative remaining cooldown and target set is ready to use, should be true" <|
            \_ ->
                Components.Skill.isReady
                    { cooldown = newMeter -100 1000
                    , name = "Skill"
                    , description = "A skill"
                    , target = Just 1
                    , effect = Damage 10
                    }
                    |> Expect.equal
                        True
        , test "Skill is off cooldown, but has no target, should not be rady" <|
            \_ ->
                Components.Skill.isReady
                    { cooldown = newMeter 0 1000
                    , name = "Skill"
                    , description = "A skill"
                    , target = Nothing
                    , effect = Damage 10
                    }
                    |> Expect.equal
                        False
        , test "Set new target equal to old, should untarget" <|
            \_ ->
                { cooldown = newMeter 0 1000
                , name = "Skill"
                , description = "A skill"
                , target = Just 1
                , effect = Damage 10
                }
                    |> Components.Skill.setTarget 1
                    |> Expect.equal
                        { cooldown = newMeter 0 1000
                        , name = "Skill"
                        , description = "A skill"
                        , target = Nothing
                        , effect = Damage 10
                        }
        ]


testColor : Components.Color.Color
testColor =
    Components.Color.initColor


color : Test
color =
    describe "Color tests"
        [ test "Create a new color with default values" <|
            \_ ->
                testColor
                    |> Expect.equal
                        { hue = newMeter 0 360
                        , saturation = newMeter 100 100
                        , lightness = newMeter 50 100
                        }
        , test "Set hue of init color to 120 (green)" <|
            \_ ->
                Components.Color.withHue 120 testColor
                    |> Expect.equal
                        { hue = newMeter 120 360
                        , saturation = testColor.saturation
                        , lightness = testColor.lightness
                        }
        , test "Set hue of init color to 400, which is above max (360), should wrap around to 40" <|
            \_ ->
                Components.Color.withHue 400 testColor
                    |> Expect.equal
                        { hue = newMeter 40 360
                        , saturation = testColor.saturation
                        , lightness = testColor.lightness
                        }
        , test "Set saturation of init color to 50" <|
            \_ ->
                Components.Color.withSaturation 50 testColor
                    |> Expect.equal
                        { hue = testColor.hue
                        , saturation = newMeter 50 100
                        , lightness = testColor.lightness
                        }
        , test "Set saturation of init color to -50, should be clamped to 0" <|
            \_ ->
                Components.Color.withSaturation -50 testColor
                    |> Expect.equal
                        { hue = testColor.hue
                        , saturation = newMeter 0 100
                        , lightness = testColor.lightness
                        }
        , test "Set lightness of init color to 0" <|
            \_ ->
                Components.Color.withLightness 0 testColor
                    |> Expect.equal
                        { hue = testColor.hue
                        , saturation = testColor.saturation
                        , lightness = newMeter 0 100
                        }
        , test "Set lightness of init color to 1000, should be clamped to 100" <|
            \_ ->
                Components.Color.withLightness 1000 testColor
                    |> Expect.equal
                        { hue = testColor.hue
                        , saturation = testColor.saturation
                        , lightness = newMeter 100 100
                        }
        , test "Get CSS string representation of init color" <|
            \_ ->
                Components.Color.toCssString testColor
                    |> Expect.equal
                        "hsl(0, 100%, 50%)"
        ]


testBuff : Buff
testBuff =
    { name = "Buff"
    , description = "A buff"
    , effects = [ Components.Stat.powerStat 3 ]
    , duration = Just (newMeter 1000 1000)
    }


testInfiniteBuff : Buff
testInfiniteBuff =
    { name = "Buff"
    , description = "A buff"
    , effects = [ Components.Stat.powerStat 3 ]
    , duration = Nothing
    }


buff : Test
buff =
    describe "Buff tests"
        [ test "Reduce duration on infinite buff" <|
            \_ ->
                testInfiniteBuff
                    |> Components.Buff.reduceDuration 16
                    |> Expect.equal
                        { name = "Buff"
                        , description = "A buff"
                        , effects = [ Components.Stat.powerStat 3 ]
                        , duration = Nothing
                        }
        , test "Reduce duration on buff" <|
            \_ ->
                testBuff
                    |> Components.Buff.reduceDuration 10
                    |> Expect.equal
                        { name = "Buff"
                        , description = "A buff"
                        , effects = [ Components.Stat.powerStat 3 ]
                        , duration = Just (newMeter 990 1000)
                        }
        , test "Check if infinite buff is active" <|
            \_ ->
                testInfiniteBuff
                    |> Components.Buff.isActive
                    |> Expect.equal
                        True
        , test "Check if buff is active" <|
            \_ ->
                testBuff
                    |> Components.Buff.reduceDuration 99999
                    |> Components.Buff.isActive
                    |> Expect.equal
                        False
        ]
