module D3.Transition (transition) where

import D3.Attributes (Attr)
import D3.Selection (Selection(..))

import Data.Maybe (Maybe(..))

transition :: forall model. Number -> Array Attr -> Selection model 
transition duration attributes = 
  Transition { label: Nothing, duration, attributes }

