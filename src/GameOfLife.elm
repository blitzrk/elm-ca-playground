module GameOfLife (evolve) where

import Array2 exposing (Array2)
import Grid exposing (Grid)
import List
import List.Extra exposing (andMap)
import Maybe.Extra


type alias Dist = Int
type alias Alive = Bool
type alias Neighborhood = List (Dist, Alive)
type alias Rule = (Alive, Neighborhood) -> Alive
type alias NeighborhoodGrid = Array2 (Alive, Neighborhood)


evolve : Grid -> Grid
evolve grid =
  Grid.fromArray2 <|
    Array2.map (applyRules rules) (neighborhood grid)


neighborhood : Grid -> NeighborhoodGrid
neighborhood grid =
  let
    locs = List.map (,) [0, 1, -1] `andMap` [0, 1, -1] |> List.drop 1

    getRel (c, r) (x, y) grid =
      Grid.get (r - y, c - x) grid

    neighbors from alive grid =
      List.map (\rel -> getRel from rel grid) locs
        |> Maybe.Extra.filter
        |> List.map ((,) 1)
        |> (,) alive
  in
     grid
      |> Grid.toArray2
      |> Array2.indexedMap neighbors
      |> Array2.map (\f -> f grid)


rules : List Rule
rules =
  let living = List.foldl (\(d,p) acc -> if p then acc + 1 else acc) 0
  in  [ (\(p, nbs) -> p && living nbs == 2)
      , (\(p, nbs) -> p && living nbs == 3)
      , (\(p, nbs) -> not p && living nbs == 3)
      ]


applyRules : List Rule -> (Alive, Neighborhood) -> Alive
applyRules rules x =
  let any = List.member True
  in  any <| List.map (\f -> f x) rules
