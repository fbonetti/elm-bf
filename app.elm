import Html exposing (Html, div, text, textarea)
import Html.Events exposing (on, targetValue)
import Html.Attributes exposing (value)
import Interpreter
import Signal

main : Signal Html
main =
  Signal.map view (Signal.foldp update init codeMailbox.signal)

-- MODEL

type alias Model =
  { input : String
  , output : String
  }

init : Model
init =
  Model "" ""

-- SIGNALS

codeMailbox : Signal.Mailbox String
codeMailbox =
  Signal.mailbox ""

-- UPDATE

update : String -> Model -> Model
update input model =
  { model |
      input <- input,
      output <- (.output << Interpreter.parse) input
  }

-- VIEW

view : Model -> Html
view model =
  div []
    [ textarea
        [ on "input" targetValue (\str -> Signal.message codeMailbox.address str)
        , value model.input
        ]
        []
    , div [] [ text model.output ]
    ]