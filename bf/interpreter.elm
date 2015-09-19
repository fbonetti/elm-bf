module Bf.Interpreter (parse) where

import Array exposing (Array)
import Char
import String
import Trampoline exposing (..)
import Debug

type alias State =
  { data : Array Int
  , dataPointer : Int
  , codePointer : Int
  , bracketCounter : Int
  , output : String
  , code : Array Char
  , input : String
  }

init : String -> String -> State
init code input
  = State (Array.fromList [0]) 0 0 0 "" ((Array.fromList << String.toList) code) input

parse : String -> String -> Result String String
parse code input =
  if | not (matchedBrackets code) -> Err "Mismatched brackets"
     | otherwise -> Ok (trampoline (parse' (init code input))).output

parse' : State -> Trampoline State
parse' state =
  if state.codePointer >= (Array.length state.code) then
    Done state
  else
    Continue (\() -> parse' (handleCommand state))

-- VALIDATIONS

matchedBrackets : String -> Bool
matchedBrackets code =
  let
    charCount char = String.filter (\c -> c == char) >> String.length
    leftBracketCount =  charCount '[' code
    rightBracketCount = charCount ']' code
  in
    leftBracketCount == rightBracketCount


-- COMMANDS

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

inputByte : State -> State
inputByte state =
  case (String.uncons state.input) of
    Just (char, rest) ->
      { state |
          input <- rest,
          data <- (Array.set state.dataPointer (Char.toCode char) state.data),
          codePointer <- state.codePointer + 1
      }      
    Nothing ->
      { state |
          data <- (Array.set state.dataPointer 0 state.data),
          codePointer <- state.codePointer + 1
      }

rewind : State -> State
rewind state =
  trampoline (rewind' state)

rewind' : State -> Trampoline State
rewind' state =
  let
    char = Array.get state.codePointer state.code
  in
    if  | char == Just ']' ->
            Continue (\() -> rewind' { state |
              bracketCounter <- state.bracketCounter + 1,
              codePointer <- state.codePointer - 1
            })
        | char == Just '[' ->
            if state.bracketCounter == 1 then
              Done { state | bracketCounter <- 0 }
            else
              Continue (\() -> rewind' { state |
                bracketCounter <- state.bracketCounter - 1,
                codePointer <- state.codePointer - 1
              })
        | otherwise ->
            Continue (\() -> rewind' { state | codePointer <- state.codePointer - 1})

fastForward : State -> State
fastForward state =
  trampoline (fastForward' state)

fastForward' : State -> Trampoline State
fastForward' state =
  let
    char = Array.get state.codePointer state.code
  in
    if  | char == Just '[' ->
            Continue (\() -> fastForward' { state |
              bracketCounter <- state.bracketCounter + 1,
              codePointer <- state.codePointer + 1
            })
        | char == Just ']' ->
            if state.bracketCounter == 1 then
              Done { state | bracketCounter <- 0 }
            else
              Continue (\() -> fastForward' { state |
                bracketCounter <- state.bracketCounter - 1,
                codePointer <- state.codePointer + 1
              })
        | otherwise ->
            Continue (\() -> fastForward' { state | codePointer <- state.codePointer + 1})

leftBracket : State -> State
leftBracket state =
  let
    byte = Array.get state.dataPointer state.data
  in
    if byte == Just 0 then 
      fastForward { state | bracketCounter <- 1, codePointer <- state.codePointer + 1 }
    else
      { state | codePointer <- state.codePointer + 1 }

rightBracket : State -> State
rightBracket state =
  let
    byte = Array.get state.dataPointer state.data
  in
    if byte == Just 0 then 
      { state | codePointer <- state.codePointer + 1 }
    else
      rewind { state | bracketCounter <- 1, codePointer <- state.codePointer - 1  }

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
      Just ',' -> inputByte state
      Just '[' -> leftBracket state
      Just ']' -> rightBracket state
      _ -> { state | codePointer <- state.codePointer + 1 }
