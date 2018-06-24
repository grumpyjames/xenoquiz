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

type Model
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
         | NextQuestion

subs model = Sub.none

update msg model =
  case msg of
    Start ->
      (GeneratingQuestion, Random.generate FetchRound (generateRound birds 4))
    FetchRound round ->
      (GeneratingQuestion, getBirdSound round)
    PickRecording round result ->
      case result of
        Err e -> Debug.crash (toString e)
        Ok o ->
          if List.isEmpty o
          then Debug.crash ("no recordings")
          else (GeneratingQuestion, Random.generate (RoundReady round) (pickRandomRecording o))
    RoundReady round result ->
      (Question round Nothing result, Cmd.none)
    SelectAnswer answer ->
      case model of
        Question round _ result  -> (Question round (Just answer) result, Cmd.none)
        _ -> Debug.crash "impossible!"
    NextQuestion ->
      (GeneratingQuestion, Random.generate FetchRound (generateRound birds 4))
    SubmitAnswer answer ->
      case model of
        Question round _ result  -> (RoundEnd round answer, Cmd.none)
        _ -> Debug.crash "impossible!"

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

view model =
  case model of
    Init -> initialView
    GeneratingQuestion -> text "Generating question"
    Question round maybeAnswer r -> showQuestion round maybeAnswer r
    RoundEnd round answer -> showRoundEnd round answer

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


