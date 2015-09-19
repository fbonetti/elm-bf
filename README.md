# elm-bf

A simple Brainfuck interpreter written entirely in Elm

```elm
parse : String -> String -> Result String String
```

Returns either an `Ok` containing the output or an `Err` containing the error message.

```elm
import Bf.Interpreter exposing (parse)
import Graphics.Element exposing (Element, show)

main : Element
main =
  case parse ">,[>,]<[.<]" "Reversed string" of
    Ok msg -> show ("Output: " ++ msg)
    Err msg -> show ("Error: " ++ msg)
```

Click here to see it in action:
http://fbonetti.github.io/elm-bf/
