module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Dict exposing (fromList)
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Maybe.Extra exposing (values)
import Random exposing (Generator, Seed, generate, list)
import Tuple exposing (first, second)


type State
    = Intro
    | InGame
    | GameWon
    | GameLost


type FieldState
    = Closed
    | Marked
    | Open


type FieldType
    = Mine
    | Empty


type Gamepad
    = Exist
    | Nonexistent


type alias Indices =
    { topLeft : Maybe Int
    , topMiddle : Maybe Int
    , topRight : Maybe Int
    , middleLeft : Maybe Int
    , middleRight : Maybe Int
    , bottomLeft : Maybe Int
    , bottomMiddle : Maybe Int
    , bottomRight : Maybe Int
    }


type alias Field =
    { fieldState : FieldState
    , fieldType : FieldType
    }



---- MODEL ----


type alias Model =
    { stateGamepad : Gamepad
    , gamepad : Array Field
    , size : ( Int, Int )
    , random : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( { stateGamepad = Nonexistent
      , gamepad = emptyGamepad
      , size = setSize
      , random = Nothing
      }
    , Cmd.none
    )


emptyGamepad : Array Field
emptyGamepad =
    Array.initialize 0 (always { fieldState = Closed, fieldType = Empty })


setSize : ( Int, Int )
setSize =
    ( 28, 18 )


initRandomGamepad : Maybe Int -> Array Field
initRandomGamepad seed =
    -- create the gamepad
    case seed of
        Just seed ->
            fillGamepadRandom seed
                (Array.initialize ((setSize |> Tuple.first) * (setSize |> Tuple.second))
                    (always { fieldState = Closed, fieldType = Empty })
                )

        Nothing ->
            emptyGamepad


fillGamepadRandom : Int -> Array Field -> Array Field
fillGamepadRandom seed gamepad =
    -- go through the array and uses "changeEmptyFields" to create a new gamepad
    Array.indexedMap (\a b -> changeEmptyFields a b seed) gamepad


changeEmptyFields : Int -> Field -> Int -> Field
changeEmptyFields index field seed =
    -- if the current field is part of the random-list the fieldType changes to mine
    if List.member index (compileRandomList seed) then
        { field | fieldState = Closed, fieldType = Mine }

    else
        { field | fieldState = Closed, fieldType = Empty }


compileRandomList : Int -> List Int
compileRandomList seed =
    -- create a list of random int with a seed
    Random.step (Random.int 0 ((setSize |> Tuple.first) * (setSize |> Tuple.second)) |> Random.list calculateNeededMines) (Random.initialSeed seed) |> Tuple.first


calculateNeededMines : Int
calculateNeededMines =
    2 * sqrt ((setSize |> Tuple.first |> toFloat) * (setSize |> Tuple.second |> toFloat)) |> round


getNumberOfMines : Array Field -> Int -> Int
getNumberOfMines gamepad index =
    -- return how many mines are in the field around
    getIndicesAround index (setSize |> Tuple.first) (setSize |> Tuple.second) |> convertIndices |> countMines gamepad


countMines : Array Field -> List Int -> Int
countMines gamepad listIndices =
    listIndices
        |> List.map (getFieldType gamepad)
        |> Maybe.Extra.values
        |> List.filter (\fieldType -> fieldType == Mine)
        |> List.length


getFieldType : Array Field -> Int -> Maybe FieldType
getFieldType gamepad index =
    -- return the FieldType or Nothing
    Maybe.map .fieldType (Array.get index gamepad)


getFieldState : Array Field -> Int -> Maybe FieldState
getFieldState gamepad index =
    -- return the FieldState or Nothing
    Maybe.map .fieldState (Array.get index gamepad)


isGameLost : Array Field -> Bool
isGameLost gamepad =
    -- if no element in the array is open and a mine the game continues else the game is lost
    if Array.isEmpty gamepad then
        True

    else if getNumberOfPredicate (\field -> field.fieldType == Mine && field.fieldState == Open) gamepad == 0 then
        False

    else
        True


isGameWon : Array Field -> Bool
isGameWon gamepad =
    -- if the game has no mine in it or all fields which are no mine are open the game is won
    getNumberOfAllMines gamepad == 0 || ((getNumberOfClosedFields gamepad == getNumberOfAllMines gamepad) && (isGameLost gamepad |> not))


getNumberOfAllMines : Array Field -> Int
getNumberOfAllMines gamepad =
    getNumberOfPredicate (\field -> field.fieldType == Mine) gamepad


getNumberOfClosedFields : Array Field -> Int
getNumberOfClosedFields gamepad =
    getNumberOfPredicate (\field -> field.fieldState == Closed || field.fieldState == Marked) gamepad


getNumberOfPredicate : (Field -> Bool) -> Array Field -> Int
getNumberOfPredicate predicate gamepad =
    -- inspect an array for a predicate and return how many statements are true
    Array.indexedMap
        (\index _ ->
            case Array.get index gamepad of
                Just field ->
                    predicate field

                Nothing ->
                    False
        )
        gamepad
        |> Array.filter identity
        |> Array.length


getIndicesAround : Int -> Int -> Int -> Indices
getIndicesAround index width height =
    -- return all indices which are next to the current field by using the width and height of the gamepad
    -- if some fields are on the edge of the gamepad it returns "Nothing" to simulate a default
    if (index > -1) && (index < (width * height)) then
        { topLeft =
            if ((index - width) > -1) && ((index % width) /= 0) then
                Just (index - width - 1)

            else
                Nothing
        , topMiddle =
            if (index - width) > -1 then
                Just (index - width)

            else
                Nothing
        , topRight =
            if ((index - width) > -1) && ((index % width) /= width - 1) then
                Just (index - width + 1)

            else
                Nothing
        , middleLeft =
            if (index % width) /= 0 then
                Just (index - 1)

            else
                Nothing
        , middleRight =
            if (index % width) /= (width - 1) then
                Just (index + 1)

            else
                Nothing
        , bottomLeft =
            if ((index % width) /= 0) && ((index + width) < (width * height)) then
                Just (index + width - 1)

            else
                Nothing
        , bottomMiddle =
            if (index + width) < (width * height) then
                Just (index + width)

            else
                Nothing
        , bottomRight =
            if ((index % width) /= (width - 1)) && ((index + width) < (width * height)) then
                Just (index + width + 1)

            else
                Nothing
        }

    else
        { topLeft = Nothing
        , topMiddle = Nothing
        , topRight = Nothing
        , middleLeft = Nothing
        , middleRight = Nothing
        , bottomLeft = Nothing
        , bottomMiddle = Nothing
        , bottomRight = Nothing
        }


convertIndices : Indices -> List Int
convertIndices indices =
    -- convert the type "Indices" to a list of int which are the index of them
    List.foldl
        (\maybe acc ->
            case maybe of
                Just a ->
                    a :: acc

                Nothing ->
                    acc
        )
        []
        [ indices.topLeft
        , indices.topMiddle
        , indices.topRight
        , indices.middleLeft
        , indices.middleRight
        , indices.bottomLeft
        , indices.bottomMiddle
        , indices.bottomRight
        ]



---- UPDATE ----


type Msg
    = ButtonMarked Int
    | ButtonClicked Int
    | Start
    | Restart
    | Roll
    | NewGame
    | NewRandom Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ButtonMarked index ->
            let
                gamepad =
                    model.gamepad

                field =
                    Array.get index model.gamepad
            in
            if isGameLost gamepad || isGameWon gamepad then
                ( model, Cmd.none )

            else
                case field of
                    Nothing ->
                        ( model, Cmd.none )

                    Just field ->
                        ( { model | gamepad = Array.set index { field | fieldState = Marked } model.gamepad }, Cmd.none )

        ButtonClicked index ->
            let
                gamepad =
                    model.gamepad

                field =
                    Array.get index model.gamepad
            in
            if isGameWon gamepad || isGameLost gamepad then
                ( model, Cmd.none )

            else
                case field of
                    Nothing ->
                        ( model, Cmd.none )

                    Just field ->
                        ( { model | gamepad = openField index model.gamepad }, Cmd.none )

        Start ->
            ( { stateGamepad = Exist
              , gamepad = initRandomGamepad model.random
              , size = setSize
              , random = model.random
              }
            , Cmd.none
            )

        Restart ->
            ( { stateGamepad = Exist
              , gamepad = initRandomGamepad model.random
              , size = setSize
              , random = model.random
              }
            , Cmd.none
            )

        Roll ->
            ( model, Random.generate NewRandom (Random.int 0 10000) )

        NewGame ->
            ( model, Random.generate NewRandom (Random.int 0 10000) )

        NewRandom newInt ->
            ( { model | random = Just newInt, stateGamepad = Nonexistent }, Cmd.none )



---- VIEW ----


getButtonClassByMineCount : Int -> String
getButtonClassByMineCount key =
    Dict.fromList [ ( 0, "zero" ), ( 1, "one" ), ( 2, "two" ), ( 3, "three" ), ( 4, "four" ), ( 5, "five" ), ( 6, "six" ), ( 7, "seven" ), ( 8, "eight" ) ]
        |> Dict.get key
        |> Maybe.withDefault "unknown"


viewField : Array Field -> Int -> Field -> Html Msg
viewField gamepad index field =
    -- describe how the field has to be shown to the user in the different cases
    case field.fieldState of
        Closed ->
            button [ onClick (ButtonMarked index), class "closed" ] [ text " C " ]

        Marked ->
            button [ onClick (ButtonClicked index), class "marked" ] [ text " X " ]

        Open ->
            case field.fieldType of
                Empty ->
                    button
                        [ onClick (ButtonClicked index)
                        , getNumberOfMines gamepad index |> getButtonClassByMineCount |> class
                        ]
                        [ if isFieldEmpty gamepad index then
                            text ""

                          else
                            getNumberOfMines gamepad index |> toString |> text
                        ]

                Mine ->
                    button [ onClick (ButtonClicked index), class "mine" ] [ text " M " ]


view : Model -> Html Msg
view model =
    case model.random of
        Nothing ->
            div []
                [ h1 [] [ text "Minesweeper" ]
                , button [ onClick Roll, class "start" ] [ text "Click here to start your game." ]
                ]

        _ ->
            case model.stateGamepad of
                Exist ->
                    div []
                        [ h1 []
                            [ text "Minesweeper" ]
                        , div []
                            (Array.indexedMap (viewField model.gamepad) model.gamepad |> viewColumn (setSize |> Tuple.first) |> Array.map viewRow |> Array.toList)
                        , if isGameWon model.gamepad then
                            div []
                                [ h1 [] [ text "Congratulations, you won the game!!!" ]
                                , button [ onClick NewGame, class "start" ] [ text "Please click here if you want to play a new game" ]
                                ]

                          else if isGameLost model.gamepad then
                            p []
                                [ h1 [] [ text "I'm sorry. You picked a Mine and lost the game. Please try again!" ]
                                , button [ onClick Restart, class "start" ] [ text "Click here to restart the game" ]
                                ]

                          else
                            p []
                                [ div [] [ text "Es sind ", getNumberOfAllMines model.gamepad |> toString |> text, text " Minen im Spielfeld versteckt!" ]
                                , div [] [ text "Auf dem Spielfeld befinden sich noch ", getNumberOfClosedFields model.gamepad |> toString |> text, text " geschlossene Felder!" ]
                                ]
                        ]

                Nonexistent ->
                    div []
                        [ h1 [] [ text "Minesweeper" ]
                        , button [ onClick Start, class "start" ] [ text "Create gamepad" ]
                        ]


viewRow : Array (Html Msg) -> Html Msg
viewRow items =
    div [] (items |> Array.toList)


viewColumn : Int -> Array (Html Msg) -> Array (Array (Html Msg))
viewColumn itemsPerLine list =
    let
        lineCount =
            Array.length list // itemsPerLine
    in
    Array.initialize lineCount
        (\index ->
            Array.slice (index * itemsPerLine) (index * itemsPerLine + itemsPerLine) list
        )


openField : Int -> Array Field -> Array Field
openField index gamepad =
    -- use "updateArray" to change the fieldType to open
    -- if the field has a mine next to him he just change this fieldState
    -- else he checks the fields next to him recursive
    let
        additionalIndicesToOpen =
            if isFieldEmpty gamepad index then
                getIndicesAround index (setSize |> Tuple.first) (setSize |> Tuple.second)
                    |> convertIndices
                    |> List.filter (fieldRequiresCheck gamepad)

            else
                []
    in
    List.foldl openField (updateArray (\f -> { f | fieldState = Open }) index gamepad) additionalIndicesToOpen


isFieldEmpty : Array Field -> Int -> Bool
isFieldEmpty gamepad index =
    -- the argument to check a field to be a Mine or next to one
    fieldPredicate (\field -> field.fieldType /= Mine && getNumberOfMines gamepad index == 0) gamepad index


fieldRequiresCheck : Array Field -> Int -> Bool
fieldRequiresCheck =
    -- the argument to check a field to be open
    fieldPredicate (\field -> field.fieldState /= Open)


fieldPredicate : (Field -> Bool) -> Array Field -> Int -> Bool
fieldPredicate predicate gamepad index =
    -- checks a field to an argument
    Array.get index gamepad
        |> Maybe.map predicate
        |> Maybe.withDefault False


updateArray : (a -> a) -> Int -> Array a -> Array a
updateArray f index array =
    -- update an array by using a function
    case Array.get index array of
        Just field ->
            Array.set index (f field) array

        Nothing ->
            array



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
