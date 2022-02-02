module Components.Color exposing (Color, initColor, toCssString, withHue, withLightness, withSaturation)

import Components.Range as Range exposing (Range, newRange)


{-| HSL color

Hue is a degree on the color wheel from 0 to 360. 0 is red, 120 is green, 240 is blue.

Saturation is a percentage value; 0% means a shade of gray and 100% is the full color.

Lightness is also a percentage; 0% is black, 100% is white.

<https://www.w3schools.com/colors/colors_hsl.asp>

-}
type alias Color =
    { hue : Range Float
    , saturation : Range Float
    , lightness : Range Float
    }


{-| Init color with default values
-}
initColor : Color
initColor =
    Color
        (newRange 0 0 360)
        (newRange 0 100 100)
        (newRange 0 50 100)


{-| Set color hue, values above 360 will be wrapped around
-}
withHue : Float -> Color -> Color
withHue hue color =
    { color | hue = Range.setValueWrap hue color.hue }


{-| Set color saturation, values will be clamped to 0-100
-}
withSaturation : Float -> Color -> Color
withSaturation saturation color =
    { color | saturation = Range.setValue saturation color.saturation }


{-| Set color lightness, values will be clamped to 0-100
-}
withLightness : Float -> Color -> Color
withLightness lightness color =
    { color | lightness = Range.setValue lightness color.lightness }


{-| Convert color to string, for use in CSS
-}
toCssString : Color -> String
toCssString color =
    -- hsl(180, 50%, 50%)
    "hsl("
        ++ (Range.getValue color.hue |> String.fromFloat)
        ++ ", "
        ++ (Range.getValue color.saturation |> String.fromFloat)
        ++ "%, "
        ++ (Range.getValue color.lightness |> String.fromFloat)
        ++ "%)"
