module Main exposing (..)

import Collage
import Color
import Element
import Html exposing (..)
import Html.Events exposing (onClick)
import Keyboard
import Random
import Time


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- constants


boardWidth : number
boardWidth =
    20


boardHeight : number
boardHeight =
    15


dotDiameter : number
dotDiameter =
    20


canvasWidth : number
canvasWidth =
    boardWidth * dotDiameter


canvasHeight : number
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
    , alive : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] 1 Nothing North True
    , Cmd.batch
        [ Random.generate NewFood randomPoint
        , Random.generate NewSnake randomPoint
        ]
    )


randomPoint : Random.Generator ( Int, Int )
randomPoint =
    Random.pair (Random.int 0 <| boardWidth - 1) (Random.int 0 <| boardHeight - 1)



-- UPDATE


type Msg
    = NewFood Point
    | NewSnake Point
    | Tick Time.Time
    | KeyDown Keyboard.KeyCode
    | RestartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RestartGame ->
            init

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
    case (List.head model.snake) of
        Nothing ->
            ( model, Cmd.none )

        Just head ->
            let
                offset =
                    relativePointForDirection model.direction

                newHead =
                    addPoints head offset |> clipPoint boardWidth boardHeight

                collidesFood =
                    case model.food of
                        Nothing ->
                            False

                        Just food ->
                            newHead == food

                collidesItself =
                    List.member newHead model.snake

                newSnakeLen =
                    if collidesFood then
                        model.snakeLength + 1
                    else
                        model.snakeLength

                tickedSnake =
                    newHead :: model.snake |> List.take newSnakeLen
            in
                if collidesItself then
                    ( { model | alive = False }, Cmd.none )
                else if collidesFood then
                    ( { model | snake = tickedSnake, snakeLength = newSnakeLen, food = Nothing }
                    , Random.generate NewFood randomPoint
                    )
                else
                    ( { model | snake = tickedSnake }, Cmd.none )


relativePointForDirection : Direction -> Point
relativePointForDirection direction =
    case direction of
        East ->
            ( 1, 0 )

        South ->
            ( 0, 1 )

        North ->
            ( 0, -1 )

        West ->
            ( -1, 0 )


addPoints : Point -> Point -> Point
addPoints ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


clipPoint : Int -> Int -> Point -> Point
clipPoint width height ( x, y ) =
    ( x % width, y % height )



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
        [ if model.alive then
            renderSnake model.snake
                ++ renderFood model.food
                |> Collage.collage canvasWidth canvasHeight
                |> Element.toHtml
          else
            h2 [] [ text "Ouuch!" ]
        , button [ onClick RestartGame ] [ text "Start over" ]
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
