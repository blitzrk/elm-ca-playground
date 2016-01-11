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

update : Input -> Model -> Model
update ({action, position} as input) ({grid, history, seed} as model) =
  let
    action' =
      if action == Toggle && not (Grid.isInside position gridShape grid)
        then None
        else action

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
  in case action' of
    Undo -> undo model
    Redo -> redo model
    Toggle -> transform <| Grid.toggle position gridShape
    Step   -> transform evolve
    IncRow -> transform <| Grid.alter Row Push
    IncCol -> transform <| Grid.alter Col Push
    DecRow -> transform <| Grid.alter Row Pop
    DecCol -> transform <| Grid.alter Col Pop
    Clear  -> transform Grid.clear
    Generate -> transformWithSeed <| Grid.randomize seed
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

gridShape = Grid.Shape 160 1 25 25 1

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
         , onClick formInput.address action
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
      toElement (gridShape.topX - 1) gridShape.topY <|
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
  in
    flow right
      [ toElement 0 0 <|
          node "link"
            [ rel "stylesheet"
            , href "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"
            ] []
      , formElem
      , Grid.draw gridShape black white darkGrey grid
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
  Signal.foldp update defaultGame inputSignal


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
      Model pattern ([], []) (Random.initialSeed 0)


-- INPUT

type alias Input =
  { action : Action
  , position : (Int, Int)
  }


clickAction : Signal Action
clickAction =
  Signal.merge
    (Signal.map (always Toggle) Mouse.clicks)
    formInput.signal


inputSignal : Signal Input
inputSignal =
  Signal.sampleOn clickAction <|
    Signal.map2 Input
      clickAction
      Mouse.position


type Action
  = None
  | Step   | Toggle
  | Clear  | Generate
  | Undo   | Redo
  | IncRow | IncCol
  | DecRow | DecCol


formInput : Signal.Mailbox Action
formInput =
  Signal.mailbox None


port animate : Signal (Task x ())
port animate =
  Animation.animate formInput.address Step
