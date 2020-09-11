module D3.Example.Force where

import Prelude

import Data.Tuple (Tuple(..))

foreign import chartFFI :: String -> Int -> Int -> String -> Unit

chart :: Tuple Int Int -> String -> Unit
chart (Tuple width height) fileContents = chartFFI "div#force" width height fileContents