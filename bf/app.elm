import Html exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (value, style, rows, href)
import Bf.Interpreter
import Signal
import String

main : Signal Html
main =
  Signal.map view (Signal.foldp update init inputs.signal)

-- MODEL

type alias Model =
  { code : String
  , input : String
  , output : String
  }

init : Model
init =
  Model "" "" ""

type Action
    = NoOp
    | SetCode String
    | SetInput String
    | SetCodeAndInput String String

-- SIGNALS

inputs : Signal.Mailbox Action
inputs =
  Signal.mailbox NoOp

-- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    SetCode code -> parseCode { model | code <- code }
    SetInput input -> parseCode { model | input <- input }
    SetCodeAndInput code input -> parseCode { model | code <- code, input <- input }

parseCode : Model -> Model
parseCode model =
  { model |
      output <-
        case (Bf.Interpreter.parse model.code model.input) of
          Ok msg -> msg
          Err msg -> msg
  }  

-- VIEW

view : Model -> Html
view model =
  div [ containerStyle ]
    [ h1 [] [ text "Brainfuck" ]
    , div [] [ text "Code:" ]
    , textarea
        [ on "input" targetValue (\str -> Signal.message inputs.address (SetCode str))
        , value model.code
        , rows 3
        , inputStyle
        ]
        []
    , div [] [ text "Input:" ]
    , textarea
        [ on "input" targetValue (\str -> Signal.message inputs.address (SetInput str))
        , value model.input
        , rows 3
        , inputStyle
        ]
        []
    , div [] [ text "Output:" ]
    , div [] [ text model.output ]
    , h3 [] [ text "Examples" ]
    , examplesList
    , instructionsTable
    ]

removeWhitespace : String -> String
removeWhitespace string =
  String.filter (\c -> c /= ' ' && c/= '\n') string

helloWorldCode : String
helloWorldCode =
  removeWhitespace
    """
    ++++++++++[>+++++++>++++++++++>+++>++++<
    <<<-]>++.>+.+++++++..+++.>>++++.<++.<+++
    +++++.--------.+++.------.--------.>+.
    """

fibonacciCode : String
fibonacciCode =
  removeWhitespace
    """
    +++++++++++>+>>>>+++++++++++++++++++++++
    +++++++++++++++++++++>++++++++++++++++++
    ++++++++++++++<<<<<<[>[>>>>>>+>+<<<<<<<-
    ]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[
    -<-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>
    [<<[>>>+<<<-]>>[-]]<<]>>>[>>+>+<<<-]>>>[
    <<<+>>>-]+<[>[-]<[-]]>[<<+>>[-]]<<<<<<<]
    >>>>>[++++++++++++++++++++++++++++++++++
    ++++++++++++++.[-]]++++++++++<[->-<]>+++
    ++++++++++++++++++++++++++++++++++++++++
    +++++.[-]<<<<<<<<<<<<[>>>+>+<<<<-]>>>>[<
    <<<+>>>>-]<-[>>.>.<<<[-]]<<[>>+>+<<<-]>>
    >[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]
    """

examplesList : Html
examplesList =
  ul []
    [ li []
        [ a 
            [ href "#"
            , onClick inputs.address (SetCodeAndInput helloWorldCode "")
            ]
            [ text "Hello World!" ]
        ]
    , li []
        [ a 
            [ href "#"
            , onClick inputs.address (SetCodeAndInput ">,[>,]<[.<]" "I'm a reversed string")
            ]
            [ text "Reverse a string" ]
        ]
    , li []
        [ a 
            [ href "#"
            , onClick inputs.address (SetCodeAndInput fibonacciCode "")
            ]
            [ text "Fibonacci sequence (warning: slow to load)" ]
        ]
    ]

instructionsTable : Html
instructionsTable =
  table [ tableStyle ]
    [ tbody []
        [ tr []
            [ th [ cellStyle [] ] [ text "Character" ]
            , th [ cellStyle [] ] [ text "Instruction" ]
            ]
        , tr []
            [ td [ cellStyle textCenter ] [ text ">" ]
            , td [ cellStyle [] ] [ text "move the data pointer to the right" ]
            ]
        , tr []
            [ td [ cellStyle textCenter ] [ text "<" ]
            , td [ cellStyle [] ] [ text "move the data pointer to the left" ]
            ]
        , tr []
            [ td [ cellStyle textCenter ] [ text "+" ]
            , td [ cellStyle [] ] [ text "increment the byte at the data pointer" ]
            ]
        , tr []
            [ td [ cellStyle textCenter ] [ text "-" ]
            , td [ cellStyle [] ] [ text "decrement the byte at the data pointer" ]
            ]
        , tr []
            [ td [ cellStyle textCenter ] [ text "." ]
            , td [ cellStyle [] ] [ text "output the byte at the data pointer" ]
            ]
        , tr []
            [ td [ cellStyle textCenter ] [ text "," ]
            , td [ cellStyle [] ] [ text "read a byte from the input" ]
            ]
        , tr []
            [ td [ cellStyle textCenter ] [ text "[" ]
            , td [ cellStyle [] ] [ text "if the current byte is 0, jump to the next matching bracket" ]
            ]
        , tr []
            [ td [ cellStyle textCenter ] [ text "]" ]
            , td [ cellStyle [] ] [ text "if the current byte is 0, rewind to the matching bracket" ]
            ]
        ]
    ]

-- STYLES

containerStyle : Attribute
containerStyle =
  style
    [ ("margin", "0 auto")
    , ("width", "600px")
    ]

inputStyle : Attribute
inputStyle =
  style [ ("width", "100%") ]

tableStyle : Attribute
tableStyle =
  style
    [ ("border-collapse", "collapse")
    , ("width", "100%")
    ]

textCenter : List (String,String)
textCenter =
  [ ("textAlign", "center") ]

cellStyle : List (String,String) -> Attribute
cellStyle options =
  style <|
    List.append
      [ ("border", "1px solid")
      , ("padding", "5px")
      ]
      options
