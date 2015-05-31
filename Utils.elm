module Utils
  ( internalMerge
  , onEnter
  , newline
  , newLineEnd
  ) where

{- |
Utility functions used by Telmnet
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JsonD
import Regex exposing (regex)

onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
  let is13 code = if code == 13 then Ok () else Err "not the right key code"
  in
    on "keydown"
         (JsonD.customDecoder keyCode is13)
         (\_ -> Signal.message address value)


internalMerge : List a -> List a -> List a
internalMerge xs ys =
  case (xs, ys) of
    (_, [])              -> xs
    ([], _)              -> ys
    (x :: xs, y :: ys)   -> x :: y :: internalMerge xs ys


newline : Regex.Regex
newline = regex "\n"

newLineEnd : Regex.Regex
newLineEnd = regex "\n$"

