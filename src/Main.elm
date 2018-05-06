module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


---- MODEL ----


type alias Model =
    { grid : Grid
    }


type alias Grid =
    List (List SquareValue)


type SquareValue
    = Empty
    | X
    | O


init : ( Model, Cmd Msg )
init =
    ( { grid = [ [ Empty, Empty, Empty ], [ Empty, Empty, Empty ], [ Empty, Empty, Empty ] ]
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateSquare SquareValue Int Int


updateGridValue : Int -> Int -> SquareValue -> Grid -> Grid
updateGridValue rowIndex colIndex squareValue grid =
    List.indexedMap
        (\rowI ->
            \row ->
                List.indexedMap
                    (\colI ->
                        \oldValue ->
                            (if rowIndex == rowI && colIndex == colI then
                                squareValue
                             else
                                oldValue
                            )
                    )
                    row
        )
        grid


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSquare newValue rowIndex colIndex ->
            ( { model | grid = (updateGridValue rowIndex colIndex newValue model.grid) }, Cmd.none )



-- ( model, Cmd.none )
---- VIEW ----


renderSquare : Int -> Int -> SquareValue -> Html Msg
renderSquare colIndex rowIndex squareValue =
    div [ class "square", onClick (UpdateSquare X rowIndex colIndex) ]
        [ text
            (case squareValue of
                X ->
                    "X"

                O ->
                    "O"

                Empty ->
                    " "
            )
        ]


renderRow : Int -> List SquareValue -> Html Msg
renderRow rowIndex row =
    div [ class "row" ]
        (List.indexedMap (\colIndex -> renderSquare colIndex rowIndex) row)


renderGrid : Grid -> Html Msg
renderGrid grid =
    div []
        (List.indexedMap
            (\rowIndex -> renderRow rowIndex)
            grid
        )


view : Model -> Html Msg
view model =
    div []
        [ renderGrid model.grid
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
