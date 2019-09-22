module Main exposing (main)

import Array exposing (Array)
import Debug
import Dict exposing (Dict)
import Browser
import Html exposing (Html, audio, button, div, source, text)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Maybe
import Random exposing (Generator)
import Result
import Url


initialSet =
    ( "UK Garden Birds", ukGardenBirds )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subs
        , view = view
        }

init : () -> (Model, Cmd Msg)
init _ = (Model initialSet (Init False), warmup)


answerSets : Dict String (Array String)
answerSets =
    Dict.fromList
        [ ( "UK Garden", ukGardenBirds )
        , ( "UK Waders", ukWaders )
        , ( "UK Woodland", ukWoodlandBirds )
        , ( "UK Reedbed", ukReedbedBirds )
        ]


ukGardenBirds =
    Array.fromList
        [ "Eurasian Blue Tit"
        , "European Robin"
        , "Dunnock"
        , "Great Tit"
        , "Chaffinch"
        , "Greenfinch"
        , "Goldfinch"
        , "Coal Tit"
        , "Long-tailed Tit"
        , "Song Thrush"
        , "Blackbird"
        , "Goldcrest"
        , "House Sparrow"
        , "Common Wood Pigeon"
        , "Eurasian Collared Dove"
        , "Eurasian Magpie"
        , "Eurasian Wren"
        ]


ukWaders =
    Array.fromList
        [ "Eurasian Curlew"
        , "Oystercatcher"
        , "Redshank"
        , "Sanderling"
        , "Dunlin"
        , "Ringed Plover"
        , "Little Ringed Plover"
        , "Avocet"
        , "Bar-Tailed Godwit"
        , "Black-Tailed Godwit"
        , "Greenshank"
        , "Common Sandpiper"
        , "Green Sandpiper"
        ]


ukWoodlandBirds =
    Array.fromList
        [ "Great Spotted Woodpecker"
        , "Lesser Spotted Woodpecker"
        , "European Green Woodpecker"
        , "Eurasian Nuthatch"
        , "Eurasian Treecreeper"
        , "Goldcrest"
        , "Common Firecrest"
        ]


ukReedbedBirds =
    Array.fromList
        [ "Bearded Reedling"
        , "Eurasian Reed Warbler"
        , "Common Whitethroat"
        , "Lesser Whitethroat"
        , "Common Grasshopper Warbler"
        , "Sedge Warbler"
        , "Savi's Warbler"
        , "Common Reed Bunting"
        , "Eurasian Wren"
        , "Cetti's Warbler"
        , "Eurasian Blackcap"
        ]


type alias Round =
    { answer : String
    , all : List String
    }


type alias Settings =
    ( String, Array String )


type alias Model =
    { settings : Settings
    , gameState : GameState
    }


type GameState
    = Init Bool
    | GeneratingQuestion
    | Question Round (Maybe String) String
    | RoundEnd Round String String


type Msg
    = Start
    | FetchRound Round
    | PickRecording Round (Result Http.Error (List String))
    | RoundReady Round String
    | SelectAnswer String
    | SubmitAnswer String
    | UseSettings Settings
    | WarmupComplete
    | NextQuestion


subs model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | gameState = GeneratingQuestion }, Random.generate FetchRound (generateRound model.settings 4) )

        FetchRound round ->
            ( model, getBirdSound round )

        PickRecording round result ->
            case result of
                Err e ->
                    Debug.todo (Debug.toString e)

                Ok o ->
                    if List.isEmpty o then
                        Debug.todo ("no recordings for " ++ round.answer)

                    else
                        ( { model | gameState = GeneratingQuestion }, Random.generate (RoundReady round) (pickRandomRecording o) )

        RoundReady round url ->
            ( { model | gameState = Question round Nothing url }, Cmd.none )

        SelectAnswer answer ->
            case model.gameState of
                Question round _ result ->
                    ( { model | gameState = Question round (Just answer) result }, Cmd.none )

                _ ->
                    Debug.todo "impossible!"

        NextQuestion ->
            ( { model | gameState = GeneratingQuestion }, Random.generate FetchRound (generateRound model.settings 4) )

        SubmitAnswer answer ->
            case model.gameState of
                Question round _ url ->
                    ( { model | gameState = RoundEnd round answer url }, Cmd.none )

                _ ->
                    Debug.todo "impossible!"

        UseSettings newSettings ->
            ( { model | settings = newSettings }, Cmd.none )

        WarmupComplete ->
            ( { model | gameState = Init True }, Cmd.none )


pickRandomRecording : List String -> Generator String
pickRandomRecording candidates =
    let
        indexGen =
            Random.int 0 (List.length candidates - 1)
    in
    Random.map (\i -> unsafeGet i (Array.fromList candidates)) indexGen


initialView ready =
    let
        attrs =
            if ready then
                [ onClick Start ]

            else
                [ Attr.disabled (not ready) ]
    in
    button attrs
        [ text
            (if ready then
                "Start Quiz"

             else
                "Please wait..."
            )
        ]


answerView : Maybe String -> String -> Html Msg
answerView maybeAnswer name =
    let
        selectedAnswer =
            Maybe.withDefault False <| Maybe.map (\t -> t == name) maybeAnswer
    in
    div
        [ Attr.class "answer"
        , Attr.class
            (if selectedAnswer then
                "selected"

             else
                "not-selected"
            )
        , onClick (SelectAnswer name)
        ]
        [ text name ]


submitAnswerView : Maybe String -> List (Html Msg)
submitAnswerView maybeAnswer =
    let
        submitButton selected =
            [ button [ onClick (SubmitAnswer selected) ] [ text "Submit Answer" ] ]
    in
    Maybe.withDefault
        [ button [ Attr.disabled True ] [ text "Submit Answer" ]
        ]
    <|
        Maybe.map submitButton maybeAnswer


birdSound url =
    audio [ Attr.controls True ] [ source [ Attr.src ("https://www.xeno-canto.org/" ++ url) ] [] ]


ui : List (Html Msg) -> List (Html Msg) -> Html Msg
ui mainPanelContent buttonPanelContent =
    div [] [ div [ Attr.class "mainPanel" ] mainPanelContent, div [ Attr.class "buttonPanel" ] buttonPanelContent ]


showQuestion : Round -> Maybe String -> String -> Html Msg
showQuestion round maybeAnswer url =
    let
        mainPanel =
            birdSound url :: List.map (answerView maybeAnswer) round.all

        buttonPanel =
            submitAnswerView maybeAnswer
    in
    ui mainPanel buttonPanel


showRoundEnd : Round -> String -> String -> Html Msg
showRoundEnd r answer url =
    let
        reveal =
            if r.answer == answer then
                [ Html.p [ Attr.class "correct" ] [ text "Correct" ] ]

            else
                [ Html.p [ Attr.class "wrong" ] [ text ("Incorrect - it was " ++ r.answer) ] ]
    in
    ui [ birdSound url, div [] reveal ] [ button [ onClick NextQuestion ] [ text "Next Bird" ] ]


unsafeLookup : Dict String b -> String -> b
unsafeLookup dict key =
    let
        v =
            Dict.get key dict
    in
    case v of
        Just val ->
            val

        Nothing ->
            Debug.todo ("not found: " ++ Debug.toString key)


selections : Settings -> Html Msg
selections ( inUse, birds ) =
    let
        optionEl ( opt, values ) =
            Html.option [ Attr.selected (opt == inUse) ] [ text opt ]

        decoder =
            Decode.map UseSettings <|
                Decode.map (\key -> ( key, unsafeLookup answerSets key )) <|
                    Html.Events.targetValue

        parentSelect =
            Html.select [ on "change" decoder, Attr.id "setSelector" ] <|
                List.map optionEl <|
                    Dict.toList answerSets

        labelForSelect =
            Html.label [ Attr.for "setSelector" ] [ text "Using set: " ]
    in
    div [ Attr.class "settings" ] [ labelForSelect, parentSelect ]


state gameState =
    case gameState of
        Init ready ->
            ui
                [ Html.h2 [] [ text "XenoQuiz" ]
                , text "A bird song quiz powered by "
                , Html.a [ Attr.href "https://www.xeno-canto.org" ] [ text "xeno-canto" ]
                ]
                [ initialView ready ]

        GeneratingQuestion ->
            ui
                [ Html.img [ Attr.src "ajax-loader.gif" ] []
                , Html.br [] []
                , Html.br [] []
                , text "Generating question"
                ]
                []

        Question round maybeAnswer r ->
            showQuestion round maybeAnswer r

        RoundEnd round answer r ->
            showRoundEnd round answer r


view : Model -> Html Msg
view model =
    div
        [ Attr.class "child" ]
        [ selections model.settings
        , div [ Attr.class "gamestate" ] [ state model.gameState ]
        ]


warmup : Cmd Msg
warmup =
    let
        url =
            "/api/birds?birdName=House Sparrow"

        request =
            Http.get url (Decode.succeed "ooh")
    in
    Http.send (\r -> WarmupComplete) request


getBirdSound : Round -> Cmd Msg
getBirdSound round =
    let
        url =
            "/api/birds?birdName=" ++ Url.percentEncode round.answer

        request =
            Http.get url decodeBirdSoundUrl
    in
    Http.send (PickRecording round) request


decodeBirdSoundUrl : Decode.Decoder (List String)
decodeBirdSoundUrl =
    let
        fileAccessor =
            Decode.at [ "file" ] Decode.string
    in
    Decode.at [ "recordings" ] <| Decode.list fileAccessor


generateRound : Settings -> Int -> Generator Round
generateRound ( name, candidates ) size =
    let
        permIndexGen =
            Random.int 0 (factorial (Array.length candidates) - 1)

        answerIndexGen =
            Random.int 0 (size - 1)

        pickRound index answerIndex =
            let
                perm =
                    Array.slice 0 size <| permutation index candidates

                answer =
                    unsafeGet answerIndex perm
            in
            Round answer <| Array.toList perm
    in
    Random.andThen (\pi -> Random.map (\ai -> pickRound pi ai) answerIndexGen) permIndexGen


factorial : Int -> Int
factorial i =
    if i == 0 then
        0

    else if i == 1 then
        1

    else
        i * factorial (i - 1)


unsafeGet : Int -> Array a -> a
unsafeGet index arr =
    let
        item =
            Array.get index arr
    in
    case item of
        Just a ->
            a

        Nothing ->
            Debug.todo ("index: " ++ Debug.toString index ++ ", array" ++ Debug.toString arr)


permutation : Int -> Array a -> Array a
permutation count input =
    let
        inputSize =
            Array.length input
    in
    if inputSize == 0 then
        Array.empty

    else
        let
            fact =
                factorial (inputSize - 1)

            itemIndex =
                count // fact

            selected =
                unsafeGet itemIndex input

            newInput =
                Array.filter (\item -> item /= selected) input
        in
        Array.push selected <|
            permutation
                (if fact == 0 then
                    0

                 else
                    modBy fact count
                )
                newInput
