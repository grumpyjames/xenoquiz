import Array
import Debug
import Json.Decode as Decode
import Html exposing (Html, button, div, text, audio, source)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import List
import Maybe
import Random
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

type Model =
  Init |
  GeneratingQuestion |
  Question String (Result Http.Error (Maybe String))

type Msg = Start
         | FetchBirdSound String
         | Bird String (Result Http.Error (Maybe String))
         | NextQuestion

subs model = Sub.none

toBird : Int -> Msg
toBird index =
    let bird = Array.get index birds
    in FetchBirdSound <| Maybe.withDefault (Debug.crash "this should never happen") bird

update msg model =
  case msg of
    Start ->
      (GeneratingQuestion, Random.generate toBird (Random.int 0 ((Array.length birds) - 1)))
    FetchBirdSound birdName ->
      (GeneratingQuestion, getBirdSound birdName)
    Bird name result ->
      (Question name result, Cmd.none)
    NextQuestion ->
      (GeneratingQuestion, Random.generate toBird (Random.int 0 ((Array.length birds) - 1)))

initialView = button [ onClick Start ] [ text "Start Quiz" ]

showQuestion : String -> Result Http.Error (Maybe String) -> Html Msg
showQuestion n r =
  case r of
    Ok val ->
      case val of
        Just url ->
          div []
            [ audio [Attr.controls True] [source [Attr.src url] []]
            , button [onClick NextQuestion] [ text "Next Bird" ]
            ]
        Nothing -> text <| "No sound found for " ++ n
    Err e ->
      text <| "Error " ++ (toString e) ++ " found attempting to find sound for " ++ n

view model =
  case model of
    Init -> initialView
    GeneratingQuestion -> text "Generating question"
    Question n r -> showQuestion n r

getBirdSound : String -> Cmd Msg
getBirdSound birdName =
  let
    url =
      "http://localhost:8080/birds?birdName=" ++ (Http.encodeUri birdName)

    request =
      Http.get url decodeBirdSoundUrl
  in
    Http.send (Bird birdName) request

decodeBirdSoundUrl : Decode.Decoder (Maybe String)
decodeBirdSoundUrl =
  let fileAccessor = Decode.at ["file"] Decode.string
  in Decode.map List.head <| Decode.at ["recordings"] <| Decode.list fileAccessor