import Json.Decode as Decode
import Html exposing (Html, button, div, text, audio, source)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import List
import Maybe
import Result

main =
  Html.program 
    { init = (Init, Cmd.none)
    , update = update
    , subscriptions = subs
    , view = view    
    }

type Model =
  Init |
  GeneratingQuestion |
  Question String (Result Http.Error (Maybe String))

type Msg = Start
         | Bird String (Result Http.Error (Maybe String))

subs model = Sub.none

update msg model =
  case msg of
    Start -> (GeneratingQuestion, getBirdSound "Eurasian Blue Tit")
    Bird name result -> (Question name result, Cmd.none)

initialView = button [ onClick Start ] [ text "Start Quiz" ]

view model =
  case model of
    Init -> initialView
    GeneratingQuestion -> text "Generating question"
    Question n r ->
      case r of
        Ok val ->
          case val of
            Just url -> audio [Attr.controls True] [source [Attr.src url] []]
            Nothing -> text <| "No sound found for " ++ n
        Err e ->
          text <| "Error " ++ (toString e) ++ " found attempting to find sound for " ++ n


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