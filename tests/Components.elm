module Components exposing (..)

import ComponentData exposing (getStat, noCompData, statCompData)
import Components.Stat exposing (StatType(..), getSumStats, hullStat, powerStat, reduceStatValue, shieldStat, statAdd, statEq)
import Expect
import Test exposing (..)


compData : Test
compData =
    describe "ComponentData tests"
        [ test "Get stat from componentData thats's a StatData variant" <|
            \_ ->
                getStat (statCompData (powerStat 0))
                    |> Expect.equal
                        (Just
                            { value = 0
                            , cap = 0
                            , statType = Power
                            }
                        )
        , test "Get stat from componentData that's not a StatData variant" <|
            \_ ->
                getStat noCompData
                    |> Expect.equal
                        Nothing
        ]


stat : Test
stat =
    describe "Stat component tests"
        [ test "Create a new hull stat with a value of 5" <|
            \_ ->
                hullStat 5
                    |> Expect.equal
                        { value = 5
                        , cap = 5
                        , statType = Hull
                        }
        , test "Create a new power stat with a value of 5" <|
            \_ ->
                powerStat 5
                    |> Expect.equal
                        { value = 5
                        , cap = 5
                        , statType = Power
                        }
        , test "Create a new shield stat with a value of 5" <|
            \_ ->
                shieldStat 5
                    |> Expect.equal
                        { value = 5
                        , cap = 5
                        , statType = Shield
                        }
        , test "Add two stats with equal type together, should return the sum of their values" <|
            \_ ->
                statAdd
                    (shieldStat 5)
                    (shieldStat 4)
                    |> Expect.equal
                        { value = 9
                        , cap = 9
                        , statType = Shield
                        }
        , test "Add two stats with non equal type together, should return the first stat" <|
            \_ ->
                statAdd
                    (shieldStat 5)
                    (powerStat 4)
                    |> Expect.equal
                        { value = 5
                        , cap = 5
                        , statType = Shield
                        }
        , test "Compare two stats of same type for equality" <|
            \_ ->
                statEq
                    (shieldStat 5)
                    (shieldStat 1)
                    |> Expect.equal
                        True
        , test "Compare two stats of different type for equality" <|
            \_ ->
                statEq
                    (shieldStat 5)
                    (powerStat 1)
                    |> Expect.equal
                        False
        , test "Get sum of power stats in a list of varius stats" <|
            \_ ->
                Components.Stat.getSumPower
                    [ powerStat 3, shieldStat 5, hullStat 1 ]
                    |> Expect.equal
                        (Just
                            { value = 3
                            , cap = 3
                            , statType = Power
                            }
                        )
        , test "Get sum of power stats in an empty list" <|
            \_ ->
                Components.Stat.getSumPower
                    []
                    |> Expect.equal
                        Nothing
        , test "Get sum of power stats in a list without any power stats" <|
            \_ ->
                Components.Stat.getSumPower
                    [ shieldStat 5, hullStat 1 ]
                    |> Expect.equal
                        Nothing
        , test "Get sum of all stats present in a list of all stats" <|
            \_ ->
                getSumStats
                    [ shieldStat 5, hullStat 1, hullStat 1, powerStat 5 ]
                    |> Expect.equal
                        [ { value = 5
                          , cap = 5
                          , statType = Shield
                          }
                        , { value = 2
                          , cap = 2
                          , statType = Hull
                          }
                        , { value = 5
                          , cap = 5
                          , statType = Power
                          }
                        ]
        , test "Get sum of all stats present in a list of some, but not all stats" <|
            \_ ->
                getSumStats
                    [ hullStat 1, hullStat 1, powerStat 5, hullStat 5 ]
                    |> Expect.equal
                        [ { value = 7
                          , cap = 7
                          , statType = Hull
                          }
                        , { value = 5
                          , cap = 5
                          , statType = Power
                          }
                        ]
        , test "Get sum of all stats present in an empty list" <|
            \_ ->
                getSumStats
                    []
                    |> Expect.equal
                        []
        , test "Reduce the value of a 5 value power stat by 2" <|
            \_ ->
                reduceStatValue 2 (powerStat 5)
                    |> Expect.equal
                        { value = 3
                        , cap = 5
                        , statType = Power
                        }
        , test "Reduce the value of a 5 value power stat by 7" <|
            \_ ->
                reduceStatValue 7 (powerStat 5)
                    |> Expect.equal
                        { value = 0
                        , cap = 5
                        , statType = Power
                        }
        , test "Reduce the value of a 5 value power stat by -7" <|
            \_ ->
                reduceStatValue -7 (powerStat 5)
                    |> Expect.equal
                        { value = 5
                        , cap = 5
                        , statType = Power
                        }
        ]
