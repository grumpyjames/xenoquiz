import Array exposing (Array)
import Debug
import Json.Decode as Decode
import Html exposing (Html, button, div, text, audio, source)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import List
import Maybe
import Random exposing (Generator)
import Result

main =
  Html.program 
    { init = (Init, Cmd.none)
    , update = update
    , subscriptions = subs
    , view = view
    }

birds =
  Array.fromList
    [ "Eurasian Blue Tit"
    , "European Robin"
    , "Dunnock"
    , "Great Tit"
    , "Eurasian Curlew"
    , "Song Thrush"
    ]

type alias Round =
  { answer: String
  , all: List String
  }

type Model =
  Init |
  GeneratingQuestion |
  Question Round (Result Http.Error (Maybe String))

type Msg = Start
         | FetchRound Round
         | RoundReady Round (Result Http.Error (Maybe String))
         | NextQuestion

subs model = Sub.none

update msg model =
  case msg of
    Start ->
      (GeneratingQuestion, Random.generate FetchRound (generateRound birds 4))
    FetchRound round ->
      (GeneratingQuestion, getBirdSound round)
    RoundReady round result ->
      (Question round result, Cmd.none)
    NextQuestion ->
      (GeneratingQuestion, Random.generate FetchRound (generateRound birds 4))

initialView = button [ onClick Start ] [ text "Start Quiz" ]

showQuestion : Round -> Result Http.Error (Maybe String) -> Html Msg
showQuestion round r =
  case r of
    Ok val ->
      case val of
        Just url ->
          div []
            ([ audio [Attr.controls True] [source [Attr.src url] []]
            , button [onClick NextQuestion] [ text "Next Bird" ]
            ] ++ List.map text (round.all))
        Nothing -> text <| "No sound found for " ++ round.answer
    Err e ->
      text <| "Error " ++ (toString e) ++ " found attempting to find sound for " ++ round.answer

view model =
  case model of
    Init -> initialView
    GeneratingQuestion -> text "Generating question"
    Question round r -> showQuestion round r

getBirdSound : Round -> Cmd Msg
getBirdSound round =
  let
    url =
      "http://localhost:8080/birds?birdName=" ++ (Http.encodeUri round.answer)
    request =
      Http.get url decodeBirdSoundUrl
  in
    Http.send (RoundReady round) request

decodeBirdSoundUrl : Decode.Decoder (Maybe String)
decodeBirdSoundUrl =
  let fileAccessor = Decode.at ["file"] Decode.string
  in Decode.map List.head <| Decode.at ["recordings"] <| Decode.list fileAccessor

generateRound : Array String -> Int -> Generator Round
generateRound candidates size =
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


