module Playground where

import Animation
import Array
import Array2 exposing (Array2)
import Color exposing (black, white, darkGrey)
import GameOfLife exposing (evolve)
import Graphics.Element exposing (..)
import Grid exposing (Grid, Dim(..), Op(..))
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Maybe exposing (withDefault)
import Mouse
import Random
import Signal
import Task exposing (Task)


main : Signal Element
main =
  Signal.map3 view
    (Signal.map (\{grid} -> grid) game)
    Animation.playing
    Animation.speed


-- UPDATE

update : Action -> Model -> Model
update action ({grid, history, seed} as model) =
  let
    transform fn =
      let grid' = fn grid
          (undos, redos) = history
          history' = (grid::undos, [])
      in  { model | grid = grid', history = history' }

    transformWithSeed fn =
      let (grid', seed') = fn grid
          (undos, redos) = history
          history' = (grid::undos, [])
      in  { model | grid = grid', history = history', seed = seed' }
  in case action of
    Undo      -> undo model
    Redo      -> redo model
    Step      -> transform <| evolve
    IncRow    -> transform <| Grid.alter Row Push
    IncCol    -> transform <| Grid.alter Col Push
    DecRow    -> transform <| Grid.alter Row Pop
    DecCol    -> transform <| Grid.alter Col Pop
    Clear     -> transform <| Grid.clear
    Generate  -> transformWithSeed <| Grid.randomize seed
    Toggle (r, c, a) -> transform <| Grid.set (r, c) a
    otherwise -> model


undo : Model -> Model
undo ({grid, history, seed} as model) =
  case history of
    ([], hs) -> model
    (g::gs, hs) ->
      { model | grid = g
      , history = (gs, grid::hs)
      }


redo : Model -> Model
redo ({grid, history, seed} as model) =
  case history of
    (gs, []) -> model
    (gs, h::hs) ->
      { model | grid = h
      , history = (grid::gs, hs)
      }



-- VIEW

(=>) = (,)

gridShape = Grid.Shape 25 25 1

view : Grid -> Animation.State -> Animation.Speed -> Element
view grid state speed =
  let
    (width, height) = Grid.dims grid

    buttonStyle extras =
      style <|
        [ "width" => "calc(100% * (1/2) - 4px)"
        , "margin" => "2px"
        , "font-size" => "85%"
        ] ++ extras

    btn attr action txt =
      [ button
        ([ class "pure-button"
         , buttonStyle []
         , onClick form.address action
         ] ++ attr
        )[ text txt ]
      ]

    tog attr txt1 txt2 =
      [ label
        ([ for "state"
         , class "pure-button pure-button-primary"
         , buttonStyle []
         ] ++ attr
        ) [ text <| if state then txt1 else txt2 ]
      , Animation.toggle "state" state
      ]

    slider name =
      [ label
        [ for name
        , style
          [ "width" => "calc(100% * (3/7) - 4px)"
          , "margin" => "2px"
          , "font-size" => "95%"
          ]
        ] [ text (name ++ ": " ++ toString speed) ]
      , Animation.slider name
        [ "width" => "calc(100% * (4/7) - 4px)"
        , "margin" => "2px"
        ]
      ]

    d = disabled True

    formElem =
      toElement 160 1 <|
        div
          [ style
            [ "display" => "flex"
            , "flex-wrap" => "wrap"
            ]
          ] <| List.concat
          [ tog []            "Stop" "Play"
          , btn []   Step     ">>"
          , slider            "Speed"
          , btn []   Clear    "Clear"
          , btn []   Generate "Random"
          , btn []   Undo     "Undo"
          , btn []   Redo     "Redo"
          , btn []   IncRow   "+ Row"
          , btn []   IncCol   "+ Col"
          , btn []   DecRow   "- Row"
          , btn []   DecCol   "- Col"
          ]

    transform x y el =
      flow right
        [ spacer x y
        , flow down
          [ spacer x y
          , el
          ]
        ]
  in
    flow right
      [ formElem
      , Grid.draw gridShape black white darkGrey grid
        |> transform 2 2
      ]


-- MODEL

type alias History = (List Grid, List Grid)

type alias Model =
  { grid : Grid
  , history : History
  , seed : Random.Seed
  }


game : Signal Model
game =
  Signal.foldp update defaultGame actions


defaultGame : Model
defaultGame =
  let t = [True]
      f = [False]
      do = List.repeat
      from = Array.fromList << List.concat
      pattern = Array.fromList << List.map from <|
        [ [do 15 False]
        , [do 3 False, do 3 True, do 3 False, do 3 True, do 3 False]
        , [do 15 False]
        , [f, t, do 4 False, t, f, t, do 4 False, t, f]
        , [f, t, do 4 False, t, f, t, do 4 False, t, f]
        , [f, t, do 4 False, t, f, t, do 4 False, t, f]
        , [do 3 False, do 3 True, do 3 False, do 3 True, do 3 False]
        ]
  in
      Model (Grid.fromArray2 pattern) ([], []) (Random.initialSeed 0)


-- INPUT

type Action
  = None   | Step   
  | Clear  | Generate
  | Undo   | Redo
  | IncRow | IncCol
  | DecRow | DecCol
  | Toggle (Int, Int, Bool)


form : Signal.Mailbox Action
form =
  Signal.mailbox None


actions : Signal Action
actions =
  Signal.merge form.signal <|
    Signal.map Toggle Grid.toggles


port animate : Signal (Task x ())
port animate =
  Animation.animate form.address Step
