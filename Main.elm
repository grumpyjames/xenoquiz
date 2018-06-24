import Array exposing (Array)
import Debug
import Json.Decode as Decode
import Html exposing (Html, button, div, text, audio, source)
import Html.Attributes as Attr
import Html.Events exposing (onClick, on)
import Http
import List
import Dict exposing (Dict)
import Maybe
import Random exposing (Generator)
import Result

main =
  Html.program 
    { init = ((Model ("UK Garden Birds", ukGardenBirds) Init), Cmd.none)
    , update = update
    , subscriptions = subs
    , view = view
    }

answerSets : Dict String (Array String)
answerSets =
  Dict.fromList
    [ ("UK Garden Birds", ukGardenBirds)
    , ("UK Waders", ukWaders)
    , ("UK Woodland Birds", ukWoodlandBirds)
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

type alias Round =
  { answer: String
  , all: List String
  }

type alias Settings = (String, Array String)

type alias Model =
  { settings: Settings
  , gameState: GameState
  }

type GameState
  = Init
  | GeneratingQuestion
  | Question Round (Maybe String) String
  | RoundEnd Round String

type Msg = Start
         | FetchRound Round
         | PickRecording Round (Result Http.Error (List String))
         | RoundReady Round String
         | SelectAnswer String
         | SubmitAnswer String
         | UseSettings Settings
         | NextQuestion

subs model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Start ->
      (model, Random.generate FetchRound (generateRound model.settings 4))
    FetchRound round ->
      (model, getBirdSound round)
    PickRecording round result ->
      case result of
        Err e -> Debug.crash (toString e)
        Ok o ->
          if List.isEmpty o
          then Debug.crash ("no recordings for " ++ round.answer)
          else ({ model | gameState = GeneratingQuestion}, Random.generate (RoundReady round) (pickRandomRecording o))
    RoundReady round url ->
      ({ model | gameState = Question round Nothing url}, Cmd.none)
    SelectAnswer answer ->
      case model.gameState of
        Question round _ result -> ({ model | gameState = Question round (Just answer) result}, Cmd.none)
        _ -> Debug.crash "impossible!"
    NextQuestion ->
      ({ model | gameState = GeneratingQuestion }, Random.generate FetchRound (generateRound model.settings 4))
    SubmitAnswer answer ->
      case model.gameState of
        Question round _ result  -> ({ model | gameState = RoundEnd round answer }, Cmd.none)
        _ -> Debug.crash "impossible!"
    UseSettings newSettings ->
      ({ model | settings = newSettings}, Cmd.none)

pickRandomRecording : List String -> Generator String
pickRandomRecording candidates =
   let indexGen = Random.int 0 ((List.length candidates) - 1)
   in Random.map (\i -> unsafeGet i (Array.fromList candidates)) indexGen

initialView = button [ onClick Start ] [ text "Start Quiz" ]

answerView : Maybe String -> String -> Html Msg
answerView maybeAnswer name  =
  let selectedAnswer = Maybe.withDefault False <| Maybe.map (\t -> t == name) maybeAnswer
  in div
    [ Attr.class "answer"
    , Attr.class (if selectedAnswer then "selected" else "not-selected")
    , onClick (SelectAnswer name) ]
    [text name]

submitAnswerView : Maybe String -> List (Html Msg)
submitAnswerView maybeAnswer =
  let submitButton selected =
    [button [onClick (SubmitAnswer selected)] [text "Submit Answer"]]
  in Maybe.withDefault
    [
      button [Attr.disabled True] [text "Submit Answer"]
    ]
    <| Maybe.map (submitButton) maybeAnswer

showQuestion : Round -> Maybe String -> String -> Html Msg
showQuestion round maybeAnswer url =
  div []
    ([
      audio [Attr.controls True] [source [Attr.src url] []]
    ] ++
      (List.map (answerView maybeAnswer) (round.all)) ++
      (submitAnswerView maybeAnswer))

showRoundEnd : Round -> String -> Html Msg
showRoundEnd r answer =
  div []
    [ div [] [if r.answer == answer then (text "Correct") else (text ("Incorrect - it was " ++ r.answer))]
    , button [onClick NextQuestion] [ text "Next Bird" ]]

unsafeLookup : Dict String b -> String -> b
unsafeLookup dict key =
  let v = Dict.get key dict
  in case v of
    Just val -> val
    Nothing -> Debug.crash ("not found: " ++ toString key)

selections : Settings -> Html Msg
selections (inUse, birds) =
  let
    optionEl (opt, values) = Html.option [ Attr.selected (opt == inUse) ] [text opt]
    decoder = Decode.map UseSettings <|
      Decode.map (\key -> (key, (unsafeLookup answerSets key))) <| Html.Events.targetValue
    parentSelect =
      Html.select [ on "change" decoder, Attr.id "setSelector" ] <|
        List.map optionEl <| Dict.toList answerSets
    labelForSelect = Html.label [Attr.for "setSelector"] [text "Using set: "]
  in div [Attr.class "settings"] [labelForSelect, parentSelect]

state gameState =
  case gameState of
    Init -> initialView
    GeneratingQuestion -> text "Generating question"
    Question round maybeAnswer r -> showQuestion round maybeAnswer r
    RoundEnd round answer -> showRoundEnd round answer

view : Model -> Html Msg
view model =
  div [] [selections model.settings, div [Attr.class "gamestate"] [state model.gameState]]

getBirdSound : Round -> Cmd Msg
getBirdSound round =
  let
    url =
      "http://localhost:8080/birds?birdName=" ++ (Http.encodeUri round.answer)
    request =
      Http.get url decodeBirdSoundUrl
  in
    Http.send (PickRecording round) request

decodeBirdSoundUrl : Decode.Decoder (List String)
decodeBirdSoundUrl =
  let fileAccessor = Decode.at ["file"] Decode.string
  in Decode.at ["recordings"] <| Decode.list fileAccessor

generateRound : Settings -> Int -> Generator Round
generateRound (name, candidates) size =
  let permIndexGen = Random.int 0 ((factorial (Array.length candidates)) - 1)
      answerIndexGen = Random.int 0 (size - 1)
      pickRound index answerIndex =
        let
          perm = Array.slice 0 size <| permutation index candidates
          answer = unsafeGet answerIndex perm
        in Round answer <| Array.toList perm
  in Random.andThen (\pi -> Random.map (\ai -> pickRound pi ai) answerIndexGen) permIndexGen

factorial : Int -> Int
factorial i =
  if i == 0 then 0 else if i == 1 then 1 else i * factorial (i - 1)

unsafeGet : Int -> Array a -> a
unsafeGet index arr =
  let item = Array.get index arr
  in case item of
    Just a -> a
    Nothing -> Debug.crash ("index: " ++ (toString index) ++ ", array" ++ (toString arr))

permutation : Int -> Array a -> Array a
permutation count input =
  let inputSize = Array.length input
  in
    if inputSize == 0
    then Array.empty
    else
      let
        fact = factorial (inputSize - 1)
        itemIndex = count // fact
        selected = unsafeGet itemIndex input
        newInput = Array.filter (\item -> item /= selected) input
      in
        Array.push selected <| permutation (if fact == 0 then 0 else count % fact) newInput


