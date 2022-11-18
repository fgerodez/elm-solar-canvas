module Solar exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (Transform(..), compositeOperationMode, transform)
import Canvas.Texture exposing (Texture, loadFromImageUrl)
import Color exposing (rgba)
import Html exposing (Html)
import Time exposing (Posix, toMillis, toSecond, utc)
import Canvas.Settings.Advanced exposing (GlobalCompositeOperationMode(..))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { seconds : Float
    , millis : Float
    , sun : Maybe Texture
    , earth : Maybe Texture
    , moon : Maybe Texture
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seconds = 0
      , millis = 0
      , sun = Nothing
      , earth = Nothing
      , moon = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Draw Posix
    | SunLoaded Texture
    | MoonLoaded Texture
    | EarthLoaded Texture
    | LoadingFailure


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw newTime ->
            ( { model
                | millis = toFloat <| toMillis utc newTime
                , seconds = toFloat <| toSecond utc newTime
              }
            , Cmd.none
            )

        SunLoaded t ->
            ( { model | sun = Just t }, Cmd.none )

        MoonLoaded t ->
            ( { model | moon = Just t }, Cmd.none )

        EarthLoaded t ->
            ( { model | earth = Just t }, Cmd.none )

        LoadingFailure ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrame Draw



-- VIEW


view : Model -> Html Msg
view model =
    let
        width =
            300

        height =
            300

        onLoad : (a -> Msg) -> Maybe a -> Msg
        onLoad msg texture =
            case texture of
                Just t ->
                    msg t

                Nothing ->
                    LoadingFailure

        textures =
            [ loadFromImageUrl "img/canvas_earth.png" (onLoad EarthLoaded)
            , loadFromImageUrl "img/canvas_moon.png" (onLoad MoonLoaded)
            , loadFromImageUrl "img/canvas_sun.png" (onLoad SunLoaded)
            ]
    in
    Canvas.toHtmlWith
        { width = width
        , height = height
        , textures = textures
        }
        []
        [ clear ( 0, 0 ) width height
        , drawScene model
        ]


renderTexture : List Setting -> Point -> Maybe Texture -> Renderable
renderTexture settings point texture =
    case texture of
        Just t ->
            Canvas.texture settings point t

        Nothing ->
            shapes [] []


drawScene : Model -> Renderable
drawScene model =
    let
        earthShadow =
            shapes [] [ rect ( 0, -12 ) 40 24 ]

        earth =
            renderTexture [] ( -12, -12 ) model.earth

        earthTrajectory =
            shapes
                [ stroke (rgba 0 153 255 0.4) ]
                [ arc ( 150, 150 ) 105 { startAngle = 0, endAngle = pi * 2, clockwise = False } ]

        moon =
            renderTexture
                [ transform
                    [ Rotate ((2 * pi / 6) * model.seconds + (2 * pi / 6000) * model.millis)
                    , Translate 0 28.5
                    ]
                ]
                ( -3.5, -3.5 )
                model.moon

        sun =
            renderTexture [] ( 0, 0 ) model.sun
    in
    group
        [ compositeOperationMode DestinationOver
        , fill (rgba 0 0 0 0.4)
        ]
        [ group
            [ transform
                [ Translate 150 150
                , Rotate ((2 * pi / 60 * model.seconds) + (2 * pi / 60000) * model.millis)
                , Translate 105 0
                ]
            ]
            [ earthShadow
            , earth
            , moon
            ]
        , earthTrajectory
        , sun
        ]
