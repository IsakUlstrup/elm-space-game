module Components.Buff exposing (Buff, isActive, newBuff, reduceDuration)

import Components.Meter exposing (Meter, newMeter)
import Components.Stat exposing (Stat)


type alias Buff =
    { name : String
    , description : String
    , effects : List Stat
    , duration : Maybe (Meter Float)
    }


{-| Buff constructor
-}
newBuff : String -> String -> List Stat -> Maybe Float -> Buff
newBuff name description effects duration =
    let
        stringWithDefault d s =
            if String.isEmpty s then
                d

            else
                s
    in
    case duration of
        Just dur ->
            Buff
                (name |> stringWithDefault "Unnamed buff")
                (description |> stringWithDefault "Buff description")
                effects
                (Just
                    (newMeter
                        dur
                        dur
                    )
                )

        Nothing ->
            Buff
                (name |> stringWithDefault "Unnamed buff")
                (description |> stringWithDefault "Buff description")
                effects
                Nothing


{-| Reduce remaining duration if not infinite
-}
reduceDuration : Float -> Buff -> Buff
reduceDuration amount buff =
    case buff.duration of
        Just duration ->
            { buff | duration = Just (Components.Meter.subtract amount duration) }

        Nothing ->
            buff


{-| Is buff active?
-}
isActive : Buff -> Bool
isActive buff =
    case buff.duration of
        Just duration ->
            Components.Meter.isEmpty duration |> not

        Nothing ->
            True
