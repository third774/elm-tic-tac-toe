module Main exposing (..)

import Html exposing (Html, text, div, h1, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Array exposing (Array)


---- MODEL ----


type alias Model =
    { grid : Grid
    , currentPlayerSymbol : Player
    , winner : Winner
    }


type alias Winner =
    Maybe Player


type alias Grid =
    Array (Array SquareValue)


type SquareValue
    = Empty
    | PlayerSymbol Player


type Player
    = X
    | O


init : ( Model, Cmd Msg )
init =
    ( { grid = Array.fromList [ Array.fromList [ Empty, Empty, Empty ], Array.fromList [ Empty, Empty, Empty ], Array.fromList [ Empty, Empty, Empty ] ]
      , currentPlayerSymbol = X
      , winner = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateSquare Int Int
    | ResetGame


updateGridValue : Int -> Int -> SquareValue -> Grid -> Grid
updateGridValue rowIndex colIndex squareValue grid =
    Array.indexedMap
        (\rowI ->
            \row ->
                if rowI == rowIndex then
                    Array.set colIndex squareValue row
                else
                    row
        )
        grid


parseGridForWinner : Grid -> Winner
parseGridForWinner grid =
    case grid |> Array.map Array.toList |> Array.toList of
        [ [ PlayerSymbol X, PlayerSymbol X, PlayerSymbol X ], _, _ ] ->
            Just X

        [ _, [ PlayerSymbol X, PlayerSymbol X, PlayerSymbol X ], _ ] ->
            Just X

        [ _, _, [ PlayerSymbol X, PlayerSymbol X, PlayerSymbol X ] ] ->
            Just X

        [ [ PlayerSymbol X, _, _ ], [ PlayerSymbol X, _, _ ], [ PlayerSymbol X, _, _ ] ] ->
            Just X

        [ [ _, PlayerSymbol X, _ ], [ _, PlayerSymbol X, _ ], [ _, PlayerSymbol X, _ ] ] ->
            Just X

        [ [ _, _, PlayerSymbol X ], [ _, _, PlayerSymbol X ], [ _, _, PlayerSymbol X ] ] ->
            Just X

        [ [ _, _, PlayerSymbol X ], [ _, PlayerSymbol X, _ ], [ PlayerSymbol X, _, _ ] ] ->
            Just X

        [ [ PlayerSymbol X, _, _ ], [ _, PlayerSymbol X, _ ], [ _, _, PlayerSymbol X ] ] ->
            Just X

        [ [ PlayerSymbol O, PlayerSymbol O, PlayerSymbol O ], _, _ ] ->
            Just O

        [ _, [ PlayerSymbol O, PlayerSymbol O, PlayerSymbol O ], _ ] ->
            Just O

        [ _, _, [ PlayerSymbol O, PlayerSymbol O, PlayerSymbol O ] ] ->
            Just O

        [ [ PlayerSymbol O, _, _ ], [ PlayerSymbol O, _, _ ], [ PlayerSymbol O, _, _ ] ] ->
            Just O

        [ [ _, PlayerSymbol O, _ ], [ _, PlayerSymbol O, _ ], [ _, PlayerSymbol O, _ ] ] ->
            Just O

        [ [ _, _, PlayerSymbol O ], [ _, _, PlayerSymbol O ], [ _, _, PlayerSymbol O ] ] ->
            Just O

        [ [ _, _, PlayerSymbol O ], [ _, PlayerSymbol O, _ ], [ PlayerSymbol O, _, _ ] ] ->
            Just O

        [ [ PlayerSymbol O, _, _ ], [ _, PlayerSymbol O, _ ], [ _, _, PlayerSymbol O ] ] ->
            Just O

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSquare rowIndex colIndex ->
            let
                updatedGrid =
                    (updateGridValue rowIndex colIndex (PlayerSymbol model.currentPlayerSymbol) model.grid)
            in
                ( { model
                    | grid = updatedGrid
                    , winner = Nothing
                    , currentPlayerSymbol =
                        case model.currentPlayerSymbol of
                            X ->
                                O

                            O ->
                                X
                  }
                , Cmd.none
                )

        ResetGame ->
            init



-- ( model, Cmd.none )
---- VIEW ----


renderSquare : Int -> Int -> SquareValue -> Html Msg
renderSquare colIndex rowIndex squareValue =
    div [ class "square", onClick (UpdateSquare rowIndex colIndex) ]
        [ text
            (case squareValue of
                PlayerSymbol X ->
                    "⚔️"

                PlayerSymbol O ->
                    "⏰"

                Empty ->
                    " "
            )
        ]


renderRow : Int -> Array SquareValue -> Html Msg
renderRow rowIndex row =
    div [ class "row" ]
        (Array.toList
            (Array.indexedMap (\colIndex -> renderSquare colIndex rowIndex) row)
        )


renderGrid : Grid -> Html Msg
renderGrid grid =
    div []
        (Array.toList
            (Array.indexedMap
                (\rowIndex -> renderRow rowIndex)
                grid
            )
        )


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "heading" ] [ text "Tic Tac Toe" ]
        , renderGrid model.grid
        , button [ class "reset", onClick ResetGame ] [ text "Reset" ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
