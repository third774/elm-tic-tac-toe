module Main exposing (..)

import Html exposing (Html, text, div, h1, button)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick)
import Dict


---- MODEL ----


type alias Model =
    { grid : Grid
    , currentPlayerSymbol : Player
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


indexedFoldl : (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldl func initialValue list =
    indexedFoldlInternal 0 func initialValue list


indexedFoldlInternal : Int -> (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldlInternal currentIndex func accumulator list =
    case ( List.head list, List.tail list ) of
        ( Just head, Just tail ) ->
            indexedFoldlInternal (currentIndex + 1) func (func currentIndex head accumulator) tail

        _ ->
            accumulator


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
                                                        |> appendToListInDict ("Horizontal" ++ toString colIndex) squareValue
                                                        |> appendToListInDict ("Vertical" ++ toString rowIndex) squareValue
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
                                allTheSameValues resultSet |> maybeMapSquareValueToWinner
                )
                Nothing


maybeMapSquareValueToWinner : Maybe SquareValue -> Winner
maybeMapSquareValueToWinner squareValue =
    case squareValue of
        Just (PlayerSymbol X) ->
            Just X

        Just (PlayerSymbol O) ->
            Just O

        _ ->
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
            "⚔️"

        O ->
            "⏰"


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
                    , winner = parseGridForWinner updatedGrid
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


view : Model -> Html Msg
view model =
    let
        winner =
            case model.winner of
                Nothing ->
                    []

                Just winner ->
                    [ h1 [] [ text (mapPlayerToString winner ++ " wins!") ] ]

        heading =
            h1
                [ class "heading" ]
                [ text "Elm Tic Tac Toe" ]
    in
        div []
            (List.concat
                [ [ heading
                  ]
                , [ renderGrid model.grid
                  , button [ class "reset", onClick ResetGame ] [ text "Reset" ]
                  ]
                , winner
                ]
            )


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
