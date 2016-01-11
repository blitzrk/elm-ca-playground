module Maybe.Extra (filter) where

import List


filter : List (Maybe a) -> List a
filter ms =
  List.foldr
    (\m acc ->
      case m of
        Nothing -> acc
        Just val -> val :: acc) [] ms
