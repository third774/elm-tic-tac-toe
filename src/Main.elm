module Main exposing (..)

import Html exposing (Html, text, div, h1, button)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick)
import Dict
import IndexedFoldl exposing (indexedFoldl)


---- MODEL ----


type alias Model =
    { grid : Grid
    , currentPlayer : Player
    , winner : Winner
    }


type alias Winner =
    Maybe Player


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
      , currentPlayer = X
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
    List.indexedMap
        (\rowI ->
            \row ->
                List.indexedMap
                    (\colI ->
                        \oldSquareValue ->
                            if rowI == rowIndex && colI == colIndex then
                                squareValue
                            else
                                oldSquareValue
                    )
                    row
        )
        grid


appendToListInDict : String -> a -> Dict.Dict String (List a) -> Dict.Dict String (List a)
appendToListInDict keyName value dict =
    case Dict.get keyName dict of
        Just list ->
            Dict.insert keyName (value :: list) dict

        Nothing ->
            Dict.insert keyName (List.singleton value) dict


parseGridForWinner : Grid -> Winner
parseGridForWinner grid =
    let
        resultsDict =
            grid
                |> indexedFoldl
                    (\rowIndex ->
                        \row ->
                            \rowResults ->
                                row
                                    |> indexedFoldl
                                        (\colIndex ->
                                            \squareValue ->
                                                \colResults ->
                                                    colResults
                                                        |> appendToListInDict ("Vertical" ++ toString colIndex) squareValue
                                                        |> appendToListInDict ("Horizontal" ++ toString rowIndex) squareValue
                                                        |> (if colIndex == rowIndex then
                                                                appendToListInDict "TopLeftToBottomRight" squareValue
                                                            else
                                                                identity
                                                           )
                                                        |> (if colIndex + rowIndex == List.length row - 1 then
                                                                appendToListInDict "BottomLeftToTopRight" squareValue
                                                            else
                                                                identity
                                                           )
                                        )
                                        rowResults
                    )
                    Dict.empty
    in
        resultsDict
            |> Dict.values
            |> List.foldl
                (\resultSet ->
                    \winner ->
                        case winner of
                            Just a ->
                                Just a

                            Nothing ->
                                allTheSameValues resultSet
                                    |> Maybe.andThen mapSquareValueToWinner
                )
                Nothing


mapSquareValueToWinner : SquareValue -> Winner
mapSquareValueToWinner squareValue =
    case squareValue of
        PlayerSymbol X ->
            Just X

        PlayerSymbol O ->
            Just O

        Empty ->
            Nothing


mapSquareValueToString : SquareValue -> String
mapSquareValueToString squareValue =
    case squareValue of
        PlayerSymbol p ->
            mapPlayerToString p

        Empty ->
            ""


mapPlayerToString : Player -> String
mapPlayerToString player =
    case player of
        X ->
            "X"

        O ->
            "O"


allTheSameValues : List a -> Maybe a
allTheSameValues list =
    case List.head list of
        Just a ->
            if list |> List.all (\x -> x == a) then
                Just a
            else
                Nothing

        _ ->
            Nothing


noMoreMoves : Grid -> Bool
noMoreMoves grid =
    List.foldl (++) [] grid
        |> List.any
            (\sv ->
                case sv of
                    Empty ->
                        True

                    _ ->
                        False
            )
        |> not


gameIsOver : Model -> Bool
gameIsOver model =
    case ( model.winner, noMoreMoves model.grid ) of
        ( Just a, _ ) ->
            True

        ( Nothing, True ) ->
            True

        _ ->
            False


invertPlayer : Player -> Player
invertPlayer currentPlayer =
    case currentPlayer of
        X ->
            O

        O ->
            X


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSquare rowIndex colIndex ->
            let
                updatedGrid =
                    updateGridValue rowIndex colIndex (PlayerSymbol model.currentPlayer) model.grid
            in
                if gameIsOver model then
                    ( model, Cmd.none )
                else
                    ( { model
                        | grid = updatedGrid
                        , winner = parseGridForWinner updatedGrid
                        , currentPlayer = invertPlayer model.currentPlayer
                      }
                    , Cmd.none
                    )

        ResetGame ->
            init



---- VIEW ----


renderSquare : Int -> Int -> SquareValue -> Html Msg
renderSquare colIndex rowIndex squareValue =
    let
        occupied =
            case squareValue of
                Empty ->
                    False

                _ ->
                    True

        className =
            if occupied then
                "square square-filled"
            else
                "square"

        clickHandler =
            if occupied then
                []
            else
                [ onClick (UpdateSquare rowIndex colIndex) ]
    in
        div
            (List.concat
                [ [ class className, attribute "aria-role" "button" ], clickHandler ]
            )
            [ text (mapSquareValueToString squareValue)
            ]


renderRow : Int -> List SquareValue -> Html Msg
renderRow rowIndex row =
    div [ class "row" ]
        (List.indexedMap (\colIndex -> renderSquare colIndex rowIndex) row)


renderGrid : Grid -> Html Msg
renderGrid grid =
    div []
        ((List.indexedMap
            (\rowIndex -> renderRow rowIndex)
            grid
         )
        )


renderWinner : Model -> Html Msg
renderWinner model =
    case ( model.winner, noMoreMoves model.grid ) of
        ( Just winner, _ ) ->
            h1 [ class "outcome" ] [ text (mapPlayerToString winner ++ " wins!") ]

        ( Nothing, True ) ->
            h1 [] [ text "Tie!" ]

        _ ->
            text ""


view : Model -> Html Msg
view model =
    let
        heading =
            h1
                [ class "heading" ]
                [ text "Elm Tic Tac Toe" ]
    in
        div [ class "container" ]
            [ heading
            , renderGrid model.grid
            , renderWinner model
            , button [ class "reset", onClick ResetGame ] [ text "Reset" ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
