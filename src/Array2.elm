module Array2
  ( Array2
  , get
  , map
  , indexedMap
  , update
  ) where

import Array exposing (Array)
import Maybe exposing (andThen)


type alias Array2 a = Array (Array a)


get : (Int, Int) -> Array2 a -> Maybe a
get (row, col) grid =
  Array.get row grid `andThen` Array.get col


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
