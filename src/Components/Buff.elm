module Components.Buff exposing (Buff, isActive, newBuff, reduceDuration)

import Components.Meter exposing (Meter)


type alias Buff e =
    { name : String
    , description : String
    , effects : List e
    , duration : Maybe (Meter Float)
    }


{-| Buff constructor
-}
newBuff : String -> String -> List e -> Maybe (Meter Float) -> Buff e
newBuff name description effects duration =
    let
        stringWithDefault d s =
            if String.isEmpty s then
                d

            else
                s
    in
    Buff
        (name |> stringWithDefault "Unnamed buff")
        (description |> stringWithDefault "Buff description")
        effects
        duration


{-| Reduce remaining duration if not infinite
-}
reduceDuration : Float -> Buff e -> Buff e
reduceDuration amount buff =
    case buff.duration of
        Just duration ->
            { buff | duration = Just (Components.Meter.subtract amount duration) }

        Nothing ->
            buff


{-| Is buff active?
-}
isActive : Buff e -> Bool
isActive buff =
    case buff.duration of
        Just duration ->
            Components.Meter.isEmpty duration |> not

        Nothing ->
            True
