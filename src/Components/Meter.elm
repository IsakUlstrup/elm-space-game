module Components.Meter exposing
    ( Meter
    , add
    , isEmpty
    , isFull
    , newMeter
    , setEmpty
    , setFull
    , subtract
    , viewHelperFloat
    , viewHelperInt
    )

{-| A meter represents a number limited to between 0 and cap

Negative cap value will be clamped to 0

-}


type Meter number
    = Meter
        { value : number
        , cap : number
        }


{-| Create new Meter
-}
newMeter : number -> number -> Meter number
newMeter curr cap =
    let
        clampedCap =
            max 0 cap
    in
    Meter
        { value = clamp 0 clampedCap curr
        , cap = clampedCap
        }


{-| Set meter value, limit to 0-cap
-}
setValue : number -> Meter number -> Meter number
setValue val (Meter meter) =
    Meter { meter | value = clamp 0 meter.cap val }


{-| Add amount to meter value
-}
add : number -> Meter number -> Meter number
add amount ((Meter meter) as m) =
    setValue (meter.value + max 0 amount) m


{-| Subtract amount from meter value
-}
subtract : number -> Meter number -> Meter number
subtract amount ((Meter meter) as m) =
    setValue (meter.value - max 0 amount) m


{-| Set meter value to 0
-}
setEmpty : Meter number -> Meter number
setEmpty (Meter meter) =
    Meter { meter | value = 0 }


{-| Set meter value to cap
-}
setFull : Meter number -> Meter number
setFull (Meter meter) =
    Meter { meter | value = meter.cap }


{-| Is meter empty? (meter value is 0)
-}
isEmpty : Meter number -> Bool
isEmpty (Meter meter) =
    meter.value == 0


{-| Is meter full? (meter value is equal to cap)
-}
isFull : Meter number -> Bool
isFull (Meter meter) =
    meter.value == meter.cap


{-| Get some values that makes rendering a float meter easier
-}
viewHelperFloat : Meter Float -> { min : String, max : String, value : String }
viewHelperFloat (Meter meter) =
    { min = "0", max = meter.cap |> String.fromFloat, value = meter.value |> String.fromFloat }


{-| Get some values that makes rendering a float meter easier
-}
viewHelperInt : Meter Int -> { min : String, max : String, value : String }
viewHelperInt (Meter meter) =
    { min = "0", max = meter.cap |> String.fromInt, value = meter.value |> String.fromInt }
