module Animation
  ( State
  , Speed
  , playing
  , toggle
  , speed
  , setSpeed
  , slider
  , animate
  ) where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Json.Encode as Json
import Maybe exposing (withDefault)
import Signal
import String
import Task exposing (Task)
import Time exposing (fps)


type alias State = Bool
type alias Speed= Int

playing = Signal.map identity play.signal
speed = Signal.map identity spd.signal


play = Signal.mailbox False
spd = Signal.mailbox 1


setSpeed : String -> Signal.Message
setSpeed str =
  let speed' = String.toInt str |> Result.toMaybe |> withDefault 1
  in  Signal.message spd.address speed'


slider id' style' =
  input
    [ type' "range"
    , id id'
    , Attr.min "1"
    , Attr.max "15"
    , property "defaultValue" (Json.string "1")
    , on "input" targetValue setSpeed
    , style style'
    ] []


toggle id' state =
  input
    [ type' "checkbox"
    , id id'
    , hidden True
    , onClick play.address (not state)
    ] []


sendWithCounter address message =
  let
    maybeStep playing t =
      if playing
        then Just <| Signal.send address message
        else Nothing

    taskCounter =
      Signal.filterMap identity (Task.succeed ()) <|
        Signal.map2 maybeStep
          playing
          (fps 60)
  in
    Signal.foldp
      (\task (acc, _) -> ((acc+1), task))
      (0, Task.succeed ())
      taskCounter


animate : Signal.Address a -> a -> Signal (Task x ())
animate address message=
  let
    scale speed =
      floor (60 * 0.8 ^ (toFloat speed - 1))

    scalePlayback (count, task) speed playing =
      if playing && count % (scale speed) == 0
        then (count // (scale speed), task)
        else (0, Task.succeed ())

    dedupCounter (t1, task) (t2, _) =
      if t1 /= t2
        then (t1, task)
        else (t1, Task.succeed ())
  in
    Signal.map snd <|
    Signal.foldp dedupCounter (-1, Task.succeed()) <|
    Signal.map3 scalePlayback
      (sendWithCounter address message)
      speed
      playing
