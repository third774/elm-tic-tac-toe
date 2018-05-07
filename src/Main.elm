module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


---- MODEL ----


type alias Model =
    { grid : Grid
    , currentPlayerSymbol : Player
    }


type alias Grid =
    List (List SquareValue)


type SquareValue
    = Empty
    | PlayerSymbol Player


type Player
    = X
    | O


init : ( Model, Cmd Msg )
init =
    ( { grid = [ [ Empty, Empty, Empty ], [ Empty, Empty, Empty ], [ Empty, Empty, Empty ] ]
      , currentPlayerSymbol = X
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateSquare Int Int


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
