module List.Extra (surround, andMap) where

import List


surround : a -> List a -> List a
surround sep xs =
  case xs of
    [] -> []
    [x] -> sep :: x :: sep :: []
    x :: xs -> sep :: x :: (surround sep xs)


andMap : List (a -> b) -> List a -> List b
andMap funcs xs =
  List.concatMap (\f -> List.map f xs) funcs
