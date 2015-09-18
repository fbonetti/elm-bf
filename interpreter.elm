module Interpreter (parse) where

import Array exposing (Array)
import Char
import String
import Maybe exposing (andThen)
import Trampoline exposing (..)

type alias State =
  { data : Array Int
  , dataPointer : Int
  , codePointer : Int
  , bracketCounter : Int
  , output : String
  , code : Array Char
  }

init : String -> State
init code
  = State (Array.fromList [0]) 0 0 0 "" ((Array.fromList << String.toList) code)

parse : String -> State
parse code = trampoline (parse' (init code))

parse' : State -> Trampoline State
parse' state =
  if state.codePointer >= (Array.length state.code) then
    Done state
  else
    Continue (\() -> parse' (handleCommand state))

incrementDataPointer : State -> State
incrementDataPointer state =
  if (state.dataPointer + 1) == Array.length state.data then
    { state | 
        data <- Array.push 0 state.data,
        dataPointer <- state.dataPointer + 1,
        codePointer <- state.codePointer + 1
    }
  else
    { state |
        dataPointer <- state.dataPointer + 1,
        codePointer <- state.codePointer + 1
    }

decrementDataPointer : State -> State
decrementDataPointer state =
  { state |
      dataPointer <- state.dataPointer - 1,
      codePointer <- state.codePointer + 1
  }

incrementByte : State -> State
incrementByte state =
  let
    current = Array.get state.dataPointer state.data
  in
    case current of
      Just value ->
        { state |
            data <- (Array.set state.dataPointer ((value + 1) % 256) state.data),
            codePointer <- state.codePointer + 1
        }
      Nothing ->
        state

decrementByte : State -> State
decrementByte state =
  let
    current = Array.get state.dataPointer state.data
  in
    case current of
      Just value ->
        { state |
            data <- (Array.set state.dataPointer ((value - 1) % 256) state.data),
            codePointer <- state.codePointer + 1
        }
      Nothing ->
        state

outputByte : State -> State
outputByte state =
  let
    current = Array.get state.dataPointer state.data
  in
    case current of
      Just byte ->
        { state |
            output <- state.output ++ ((String.fromChar << Char.fromCode) byte),
            codePointer <- state.codePointer + 1
        }
      Nothing ->
        state

rewind : State -> State
rewind state =
  trampoline (rewind' state)

rewind' : State -> Trampoline State
rewind' state =
  let
    char = Array.get state.codePointer state.code
    byte = Array.get state.dataPointer state.data
  in
    if | byte == Just 0 -> Done state
       | char == Just ']' ->
           Continue (\() -> rewind' { state |
              bracketCounter <- state.bracketCounter + 1,
              codePointer <- state.codePointer - 1
            })
        | char == Just '[' ->
            Continue (\() -> rewind' { state |
              bracketCounter <- state.bracketCounter - 1,
              codePointer <- state.codePointer - 1
            })
        | otherwise ->
            Continue (\() -> rewind' { state | codePointer <- state.codePointer - 1})

handleCommand : State -> State
handleCommand state =
  let
    char = Array.get state.codePointer state.code
  in
    case char of
      Just '>' -> incrementDataPointer state
      Just '<' -> decrementDataPointer state
      Just '+' -> incrementByte state
      Just '-' -> decrementByte state
      Just '.' -> outputByte state
      Just ']' -> rewind state
      _ -> { state | codePointer <- state.codePointer + 1 }
