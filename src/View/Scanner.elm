module Scanner exposing (viewScanner)

import Html exposing (Html, div)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


width : number
width =
    100


height : number
height =
    100


glowFilter : Svg msg
glowFilter =
    Svg.filter [ SvgAttr.id "glow", SvgAttr.width "500%", SvgAttr.height "500%", SvgAttr.x "-250%", SvgAttr.y "-250%" ]
        [ Svg.feGaussianBlur [ SvgAttr.class "blur", SvgAttr.result "coloredBlur", SvgAttr.stdDeviation "0.15" ] []
        , Svg.feMerge []
            [ Svg.feMergeNode [ SvgAttr.in_ "coloredBlur" ] []
            , Svg.feMergeNode [ SvgAttr.in_ "coloredBlur" ] []
            , Svg.feMergeNode [ SvgAttr.in_ "SourceGraphic" ] []
            ]
        ]


positionToString : ( Int, Int ) -> String
positionToString ( tx, ty ) =
    String.fromInt tx ++ ", " ++ String.fromInt ty


lookAt : ( Int, Int ) -> ( Int, Int ) -> Float
lookAt ( x, y ) ( tx, ty ) =
    let
        ( dx, dy ) =
            ( tx - x |> toFloat, ty - y |> toFloat )
    in
    atan2 dy dx
        |> (\rad -> rad * (180 / pi) + 90)


polyLineBurn : List ( Int, Int ) -> List (Svg msg)
polyLineBurn points =
    let
        pointsAttr ps =
            SvgAttr.points
                (ps
                    |> List.map positionToString
                    |> List.intersperse " "
                    |> String.concat
                )

        circle p =
            Svg.circle
                [ SvgAttr.class "vector-path-burn"
                , SvgAttr.cx (Tuple.first p |> String.fromInt)
                , SvgAttr.cy (Tuple.second p |> String.fromInt)
                , SvgAttr.r "0.07"
                ]
                []
    in
    Svg.polyline [ pointsAttr points ] [] :: List.map circle points


ship : List (Svg msg)
ship =
    polyLineBurn [ ( -3, 3 ), ( 0, -3 ), ( 3, 3 ), ( 0, 1 ), ( -3, 3 ) ]


drone : List (Svg msg)
drone =
    polyLineBurn [ ( 0, 0 ) ]


destination : List (Svg msg)
destination =
    polyLineBurn [ ( -2, 2 ), ( -2, -2 ), ( 2, -2 ), ( 2, 2 ), ( -2, 2 ) ]


asteroid : List (Svg msg)
asteroid =
    polyLineBurn [ ( -4, 0 ), ( -2, -2 ), ( 0, -2 ), ( 4, 0 ), ( 3, 3 ), ( 0, 4 ), ( -4, 0 ) ]


spaceEntity : ( Int, Int ) -> Maybe ( Int, Int ) -> List (Svg msg) -> Svg msg
spaceEntity ( x, y ) target children =
    let
        translation t =
            case t of
                Just ( tx, ty ) ->
                    "translate("
                        ++ String.fromInt (width // 2 + x)
                        ++ ", "
                        ++ String.fromInt (height // 2 + y)
                        ++ "), "
                        ++ "rotate("
                        ++ (lookAt ( x, y ) ( tx, ty ) |> String.fromFloat)
                        ++ ")"

                Nothing ->
                    "translate("
                        ++ String.fromInt (width // 2 + x)
                        ++ ", "
                        ++ String.fromInt (height // 2 + y)
                        ++ ")"
    in
    Svg.g
        [ SvgAttr.transform (translation target), SvgAttr.class "vector-path" ]
        children


entityAtAngle : Float -> Float -> Maybe ( Int, Int ) -> List (Svg msg) -> Svg msg
entityAtAngle radius angle target children =
    let
        ( shipX, shipY ) =
            ( cos (radians angle) * radius
            , sin (radians angle) * radius
            )
    in
    spaceEntity ( round shipX, round shipY ) target children


shipEntity : Float -> Maybe ( Int, Int ) -> Svg msg
shipEntity angle target =
    entityAtAngle 30
        angle
        target
        ship


droneEntity : Float -> Svg msg
droneEntity angle =
    entityAtAngle 8 angle Nothing drone


asteroidEntity : Float -> Svg msg
asteroidEntity angle =
    entityAtAngle 30 angle Nothing asteroid


destinationEntity : Float -> Svg msg
destinationEntity angle =
    entityAtAngle 45 angle (Just ( 0, 0 )) destination


viewScanner : Html msg
viewScanner =
    div [ Html.Attributes.class "vector-display-container" ]
        [ Svg.svg [ SvgAttr.viewBox "0 0 100 100", SvgAttr.class "vector-display" ]
            [ glowFilter
            , spaceEntity ( 0, 0 ) Nothing ship
            , droneEntity 1
            , droneEntity -2
            , shipEntity 0 (Just ( 0, 0 ))
            , shipEntity 3 Nothing
            , shipEntity 4 (Just ( 0, 0 ))
            , shipEntity 5 Nothing
            , asteroidEntity 1
            , asteroidEntity 2
            , destinationEntity 1
            , destinationEntity -1
            , destinationEntity 3.5
            ]
        ]
