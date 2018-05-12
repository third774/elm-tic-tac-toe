module Main exposing (..)

import Html exposing (Html, text, div, h1, button)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick)
import Array exposing (Array)
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


indexWithValue : Array a -> Array ( Int, a )
indexWithValue list =
    Array.indexedMap (\i -> \v -> ( i, v )) list


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
        emptyDict : Dict.Dict String (List SquareValue)
        emptyDict =
            Dict.empty

        resultsDictValues =
            grid
                |> indexWithValue
                |> Array.foldl
                    (\( rowIndex, row ) ->
                        \rowResults ->
                            row
                                |> indexWithValue
                                |> Array.foldl
                                    (\( colIndex, squareValue ) ->
                                        \colResults ->
                                            let
                                                nonDiagonalResults =
                                                    colResults
                                                        |> appendToListInDict ("Horizontal" ++ toString colIndex) squareValue
                                                        |> appendToListInDict ("Vertical" ++ toString rowIndex) squareValue

                                                withDiagonalResults =
                                                    if colIndex == rowIndex then
                                                        nonDiagonalResults |> appendToListInDict ("TopLeftToBottomRight") squareValue
                                                    else if (colIndex + rowIndex) == (Array.length row) - 1 then
                                                        nonDiagonalResults |> appendToListInDict ("BottomLeftToTopRight") squareValue
                                                    else
                                                        nonDiagonalResults
                                            in
                                                withDiagonalResults
                                    )
                                    rowResults
                    )
                    emptyDict
                |> Dict.values

        _ =
            resultsDictValues |> toString |> Debug.log "results"
    in
        resultsDictValues
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
                currentValue =
                    Array.get rowIndex model.grid |> Maybe.andThen (Array.get colIndex) |> Maybe.withDefault Empty
            in
                case currentValue of
                    Empty ->
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

                    _ ->
                        ( model, Cmd.none )

        ResetGame ->
            init



-- ( model, Cmd.none )
---- VIEW ----


renderSquare : Int -> Int -> SquareValue -> Html Msg
renderSquare colIndex rowIndex squareValue =
    let
        className =
            case squareValue of
                Empty ->
                    "square"

                option2 ->
                    "square square-filled"
    in
        div [ class className, attribute "aria-role" "button", onClick (UpdateSquare rowIndex colIndex) ]
            [ text
                (case squareValue of
                    PlayerSymbol X ->
                        "⚔️"

                    PlayerSymbol O ->
                        "⏰"

                    Empty ->
                        ""
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
    let
        winner =
            case model.winner of
                Nothing ->
                    []

                Just winner ->
                    [ h1 [] [ text (toString winner ++ " wins!") ] ]

        heading =
            h1
                [ class "heading" ]
                [ text "Tic Tac Toe" ]
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



-- ((
--  )
--     :: winner
-- )
---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
