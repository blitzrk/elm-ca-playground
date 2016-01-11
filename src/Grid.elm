module Grid
  ( Grid, Shape, Dim(..), Op(..)
  , map
  , indexedMap
  , get
  , dims
  , isInside
  , alter
  , toggle
  , clear
  , randomize
  , draw
  ) where

import Array
import Array2 exposing (Array2)
import Color exposing (Color)
import Graphics.Element as Element exposing (Element, flow, down, right, spacer)
import List.Extra exposing (surround)
import Random exposing (generate)

type alias Grid = Array2 Bool

type alias Shape =
  { topX : Int
  , topY : Int
  , cellW : Int
  , cellH : Int
  , gridLine : Int
  }


map = Array2.map
indexedMap = Array2.indexedMap
get = Array2.get


dims : Grid -> (Int, Int)
dims grid =
  let
    y = Array.length grid
    x = grid
      |> Array.get 0
      |> Maybe.map Array.length
      |> Maybe.withDefault 0
  in
    (x, y)


isInside: (Int, Int) -> Shape -> Grid -> Bool
isInside pos shape grid =
  getCell pos shape grid /= Nothing


type alias Cell = { row : Int , col : Int }

getCell : (Int, Int) -> Shape -> Grid -> Maybe Cell
getCell (x, y) shape grid =
  let
    relX = x - (shape.topX + 1)
    relY = y - (shape.topY + 1)
    row = relY // (shape.cellH + shape.gridLine)
    col = relX // (shape.cellW + shape.gridLine)
    (cols, rows) = dims grid
  in
    if row < rows && col < cols && Basics.min relX relY >= 0
      then Just <| Cell row col
      else Nothing


maybeToggle : Grid -> Maybe Cell -> Grid
maybeToggle grid maybe =
  case maybe of
    Nothing -> grid
    Just {row, col} ->
      Array2.update (row, col) not grid


toggle : (Int, Int) -> Shape -> Grid -> Grid
toggle pos shape grid =
  getCell pos shape grid
    |> maybeToggle grid


type Dim = Row | Col
type Op = Push | Pop

alter : Dim -> Op -> Grid -> Grid
alter dim op grid =
  let
    (ncol, nrow) = dims grid
  in
    case (dim, op) of
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
  Array2.map (always False)


randomize : Random.Seed -> Grid -> (Grid, Random.Seed)
randomize seed grid =
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
  in
    Array.foldl accumRandArrays (Array.empty, seed) grid


draw : Shape -> Color -> Color -> Color -> Grid -> Element
draw shape fgColor bgColor gridColor grid =
  let
    (w, _) = dims grid

    width = shape.gridLine + w * (shape.cellW + 1)

    rowLine =
      spacer width shape.gridLine
        |> Element.color gridColor

    colLine =
      spacer shape.gridLine shape.cellH
        |> Element.color gridColor

    drawCell alive =
      spacer shape.cellW shape.cellH
        |> Element.color (if alive then fgColor else bgColor)

    drawRow =
      flow right <<
        surround colLine << Array.toList << Array.map drawCell
  in
    flow down <|
      surround rowLine <| Array.toList <| Array.map drawRow grid
