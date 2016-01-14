module Array2
  ( Array2
  , dims
  , get
  , set
  , map
  , indexedMap
  , update
  ) where

import Array exposing (Array)
import Maybe exposing (withDefault, andThen)


type alias Array2 a = Array (Array a)


dims : Array2 a -> (Int, Int)
dims array2 =
  let
    y = Array.length array2
    x = array2
      |> Array.get 0
      |> Maybe.map Array.length
      |> Maybe.withDefault 0
  in
    (x, y)


get : (Int, Int) -> Array2 a -> Maybe a
get (row, col) grid =
  Array.get row grid `andThen` Array.get col


set : (Int, Int) -> a -> Array2 a -> Array2 a
set (row, col) to =
  Array.indexedMap <| \r arr ->
    if r == row
      then (Array.set col to arr)
      else arr


map : (a -> b) -> Array2 a -> Array2 b
map f =
  indexedMap (always f)


indexedMap : ((Int, Int) -> a -> b) -> Array2 a -> Array2 b
indexedMap f rows =
  let
    withIndex row acc =
      let y = Array.length acc
      in  Array.push (Array.indexedMap (\x v -> f (x, y) v) row) acc
  in
    Array.foldl withIndex Array.empty rows


update : (Int, Int) -> (a -> a) -> Array2 a -> Array2 a
update (row, col) fn =
  let
    update1d index fn array =
      case Array.get index array of
        Nothing -> array
        Just val ->
          Array.set index (fn val) array
  in
    update1d row <| update1d col fn
