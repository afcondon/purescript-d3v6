module D3.Example.Tree where

import Prelude

import Affjax (Error)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

foreign import chartFFI :: String -> Number -> Number -> String -> Unit

chart :: forall r. Tuple Number Number -> Either Error { body ∷ String | r } -> Unit
chart (Tuple width height) (Right { body } ) = chartFFI "div#tree" width height body
chart _               (Left error)      = unit

{- AS IS version of script
  process data using d3.hierarchy (NB non-PureScript data structure, no children field on )

-}