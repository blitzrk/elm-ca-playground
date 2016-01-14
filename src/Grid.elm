module Grid
  ( Grid, Shape, Dim(..), Op(..)
  , empty
  , init
  , fromArray2
  , map
  , indexedMap
  , get
  , set
  , toggle
  , dims
  , alter
  , clear
  , randomize
  , draw
  , toggles
  ) where


import Array
import Array2 exposing (Array2)
import Color exposing (Color)
import Graphics.Element as Element exposing (Element, flow, down, right, spacer)
import Graphics.Input as Input
import List.Extra exposing (surround)
import Mouse
import Random exposing (generate)


type Grid = Grid (Array2 Bool)

type alias Shape =
  { cellW : Int
  , cellH : Int
  , gridLine : Int
  }


empty : Grid
empty =
  Grid Array.empty


init : Int -> Int -> Grid
init rows cols =
  Grid <| Array.repeat rows <| Array.repeat cols False


fromArray2 : Array2 Bool -> Grid
fromArray2 array2
  = Grid array2


map : (Bool -> Bool) -> Grid -> Grid
map fn (Grid grid) =
  Grid <| Array2.map fn grid


indexedMap : ((Int, Int) -> Bool -> Bool) -> Grid -> Grid
indexedMap fn (Grid grid) =
  Grid <| Array2.indexedMap fn grid


get : (Int, Int) -> Grid -> Maybe Bool
get pos (Grid grid) =
  Array2.get pos grid


set : (Int, Int) -> Bool -> Grid -> Grid
set pos to (Grid grid) =
  Grid <| Array2.set pos to grid


toggle : (Int, Int) -> Grid -> Grid
toggle pos (Grid grid) =
  Grid <| Array2.update pos not grid


dims : Grid -> (Int, Int)
dims (Grid grid) =
  Array2.dims grid


type Dim = Row | Col
type Op = Push | Pop

alter : Dim -> Op -> Grid -> Grid
alter dim op ((Grid grid) as g) =
  let
    (ncol, nrow) = dims g
  in
    Grid <| case (dim, op) of
      (Row, Push) ->
        Array.push (Array.repeat ncol False) grid

      (Row, Pop) ->
        Array.slice 0 (nrow - 1) grid

      (Col, Push) ->
        Array.map (Array.push False) grid

      (Col, Pop) ->
        Array.map (Array.slice 0 (ncol - 1)) grid


clear : Grid -> Grid
clear =
  map (always False)


randomize : Random.Seed -> Grid -> (Grid, Random.Seed)
randomize seed (Grid grid) =
  let
    accumRand p (acc, seed) =
      let
        (p', seed') = generate Random.bool seed
        acc' = Array.push p' acc
      in
        (acc', seed')

    randomizeRow seed row =
      Array.foldl accumRand (Array.empty, seed) row

    accumRandArrays row (acc, seed) =
      let
        (row', seed') = randomizeRow seed row
        acc' = Array.push row' acc
      in
        (acc', seed')

    (grid', seed') = Array.foldl accumRandArrays (Array.empty, seed) grid
  in
    (Grid grid', seed')


draw : Shape -> Color -> Color -> Color -> Grid -> Element
draw shape fgColor bgColor gridColor ((Grid grid) as g) =
  let
    (w, _) = dims g

    width = shape.gridLine + w * (shape.cellW + 1)

    rowLine =
      spacer width shape.gridLine
        |> Element.color gridColor

    colLine =
      spacer shape.gridLine shape.cellH
        |> Element.color gridColor

    drawCell row col alive =
      spacer shape.cellW shape.cellH
        |> Element.color (if alive then fgColor else bgColor)
        |> Input.clickable (Signal.message click.address <| At (row, col) (not alive))
        |> Input.hoverable (Signal.message hover.address << always (At (row,col) (not alive)))

    drawRow n =
      flow right <<
        surround colLine << Array.toList << Array.indexedMap (drawCell n)
  in
    flow down <|
      surround rowLine <| Array.toList <| Array.indexedMap drawRow grid


toggles : Signal Toggle
toggles =
  Signal.merge click.signal <|
    Signal.map2 (\down tog -> if down then tog else None)
      Mouse.isDown
      hover.signal


-- Helpers for interaction

type Toggle = None | At (Int, Int) Bool

click =
  Signal.mailbox <| At (-1, -1) False

hover =
  Signal.mailbox <| At (-1, -1) False
