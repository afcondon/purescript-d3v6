module D3.Example.Tree where

import Prelude

import Data.Tuple (Tuple(..))

foreign import chartFFI :: String -> Int -> Int -> String -> Unit

chart :: Tuple Int Int -> String -> Unit
chart (Tuple width height) fileContents = chartFFI "div#tree" width height fileContents

{- AS IS version of script
  process data using d3.hierarchy (NB non-PureScript data structure, no children field on )

-}