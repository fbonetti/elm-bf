module Interpreter (parse) where

import Array exposing (Array)
import Char
import String
import Maybe exposing (andThen)

type alias State =
  { data : Array Int
  , dataPointer : Int
  , output : String
  }

init : State
init = State (Array.fromList [0]) 0 ""

--parse : String -> State
--parse list = trampoline (length' 0 list)

--length' : Int -> List a -> Trampoline Int
--length' accum list =
--    case list of
--      []     -> Done accum
--      hd::tl -> Continue (\() -> length' (accum+1) tl)

parse : String -> State
parse code =
  String.foldl handleCommand init code

incrementDataPointer : State -> State
incrementDataPointer state =
  if (state.dataPointer + 1) == Array.length state.data then
    { state | 
        data <- Array.push 0 state.data,
        dataPointer <- state.dataPointer + 1
    }
  else
    { state | dataPointer <- state.dataPointer + 1 }

decrementDataPointer : State -> State
decrementDataPointer state =
  { state | dataPointer <- state.dataPointer - 1 }

incrementByte : State -> State
incrementByte state =
  let
    current = Array.get state.dataPointer state.data
  in
    case current of
      Just value ->
        { state | data <- (Array.set state.dataPointer ((value + 1) % 256) state.data) }
      Nothing ->
        state

decrementByte : State -> State
decrementByte state =
  let
    current = Array.get state.dataPointer state.data
  in
    case current of
      Just value ->
        { state | data <- (Array.set state.dataPointer ((value - 1) % 256) state.data) }
      Nothing ->
        state

outputByte : State -> State
outputByte state =
  let
    current = Array.get state.dataPointer state.data
  in
    case current of
      Just byte ->
        { state | output <- state.output ++ ((String.fromChar << Char.fromCode) byte) }
      Nothing ->
        state

handleCommand : Char -> State -> State
handleCommand char state =
  case char of
    '>' -> incrementDataPointer state
    '<' -> decrementDataPointer state
    '+' -> incrementByte state
    '-' -> decrementByte state
    '.' -> outputByte state
    _ -> state
