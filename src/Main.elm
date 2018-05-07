module Main exposing (..)

import Html exposing (Html, text, div, h1, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Array exposing (Array)


---- MODEL ----


type alias Model =
    { grid : Grid
    , currentPlayerSymbol : Player
    , winner : Maybe Player
    }


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
      , winner = Maybe.Nothing
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSquare rowIndex colIndex ->
            ( { model
                | grid = (updateGridValue rowIndex colIndex (PlayerSymbol model.currentPlayerSymbol) model.grid)
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
