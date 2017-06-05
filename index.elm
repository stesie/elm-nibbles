module Main exposing (..)

import Collage
import Color
import Element
import Html exposing (..)
import Keyboard
import Random
import Time


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- constants


boardWidth =
    20


boardHeight =
    15


dotDiameter =
    20


canvasWidth =
    boardWidth * dotDiameter


canvasHeight =
    boardHeight * dotDiameter



-- MODEL


type alias Point =
    ( Int, Int )


type Direction
    = East
    | North
    | West
    | South


type alias Model =
    { snake : List Point
    , snakeLength : Int
    , food : Maybe Point
    , direction : Direction
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] 3 Nothing North
    , Cmd.batch
        [ Random.generate NewFood randomPoint
        , Random.generate NewSnake randomPoint
        ]
    )


randomPoint : Random.Generator ( Int, Int )
randomPoint =
    Random.pair (Random.int 0 boardWidth) (Random.int 0 boardHeight)



-- UPDATE


type Msg
    = NewFood Point
    | NewSnake Point
    | Tick Time.Time
    | KeyDown Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewFood food ->
            ( { model | food = Just food }, Cmd.none )

        NewSnake snake ->
            ( { model | snake = [ snake ] }, Cmd.none )

        Tick _ ->
            tickSnake model

        KeyDown keyCode ->
            case (keyCodeToDirection keyCode) of
                Just dir ->
                    ( { model | direction = dir }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


keyCodeToDirection : Keyboard.KeyCode -> Maybe Direction
keyCodeToDirection x =
    case x of
        37 ->
            Just West

        38 ->
            Just North

        39 ->
            Just East

        40 ->
            Just South

        _ ->
            Nothing


tickSnake : Model -> ( Model, Cmd Msg )
tickSnake model =
    let
        offset =
            case model.direction of
                East ->
                    ( 1, 0 )

                South ->
                    ( 0, 1 )

                North ->
                    ( 0, -1 )

                West ->
                    ( -1, 0 )

        tickedSnake =
            case (List.head model.snake) of
                Nothing ->
                    model.snake

                Just point ->
                    (addPoints point offset) :: model.snake |> List.take model.snakeLength
    in
        ( { model | snake = tickedSnake }, Cmd.none )


addPoints : Point -> Point -> Point
addPoints ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second Tick
        , Keyboard.downs KeyDown
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ renderSnake model.snake
            ++ renderFood model.food
            |> Collage.collage canvasWidth canvasHeight
            |> Element.toHtml
        ]


renderSnake : List Point -> List Collage.Form
renderSnake =
    List.map (renderPoint Color.green)


renderFood : Maybe Point -> List Collage.Form
renderFood x =
    case x of
        Nothing ->
            []

        Just p ->
            [ renderPoint Color.red p ]


renderPoint : Color.Color -> Point -> Collage.Form
renderPoint color ( x, y ) =
    Collage.circle 10
        |> Collage.filled color
        |> Collage.move
            ( toFloat x * dotDiameter - canvasWidth / 2 + dotDiameter / 2
            , canvasHeight - toFloat y * dotDiameter - canvasHeight / 2 - dotDiameter / 2
            )
