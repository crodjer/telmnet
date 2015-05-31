module Utils
  ( internalMerge
  , onEnter
  , newline
  , newLineEnd
  , splitAround
  , splitAroundNewline
  ) where

{- |
Utility functions used by Telmnet.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JsonD
import Regex exposing (regex, find, split)

{- | Event triggered when enter is pressed. Borrowed from Elm TodoMVC's Todo.elm
-}
onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
  let is13 code = if code == 13 then Ok () else Err "not the right key code"
  in
    on "keydown"
         (JsonD.customDecoder keyCode is13)
         (\_ -> Signal.message address value)


{- | Merge two lists while taking values from each alternatively.

internalMerge [1, 3, 5] [2, 4]
> [1, 2, 3, 4, 5]
-}
internalMerge : List a -> List a -> List a
internalMerge xs ys =
  case (xs, ys) of
    (_, [])              -> xs
    ([], _)              -> ys
    (x :: xs, y :: ys)   -> x :: y :: internalMerge xs ys


{- | A regex to match newlines.
-}
newline : Regex.Regex
newline = regex "\n"

{- | A regex to match newlines in the end.
-}
newLineEnd : Regex.Regex
newLineEnd = regex "\n$"

{- | Split around the provided regex. Conserves the regex matched slice.
-}
splitAround : Regex.Regex -> String -> List String
splitAround rx str =
  internalMerge (split Regex.All rx str) (List.map .match (find Regex.All rx str))

{- | Split around a newline, specifically.
-}
splitAroundNewline : String -> List String
splitAroundNewline = splitAround newline
