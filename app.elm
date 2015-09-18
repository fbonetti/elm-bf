import Html exposing (Html, div, text, textarea)
import Html.Events exposing (on, targetValue)
import Html.Attributes exposing (value)
import Interpreter
import Signal

main : Signal Html
main =
  Signal.map view (Signal.foldp update init (Signal.map2 (,) codeMailbox.signal inputMailbox.signal))

-- MODEL

type alias Model =
  { code : String
  , input : String
  , output : String
  }

init : Model
init =
  Model "" "" ""

-- SIGNALS

inputMailbox : Signal.Mailbox String
inputMailbox =
  Signal.mailbox ""

codeMailbox : Signal.Mailbox String
codeMailbox =
  Signal.mailbox ""

-- UPDATE

update : (String,String) -> Model -> Model
update (code,input) model =
  { model |
      code <- code,
      input <- input,
      output <- (Interpreter.parse code input).output
  }

-- VIEW

view : Model -> Html
view model =
  div []
    [ textarea
        [ on "input" targetValue (\str -> Signal.message codeMailbox.address str)
        , value model.code
        ]
        []
    , textarea
        [ on "input" targetValue (\str -> Signal.message inputMailbox.address str)
        , value model.input
        ]
        []
    , div [] [ text model.output ]
    ]