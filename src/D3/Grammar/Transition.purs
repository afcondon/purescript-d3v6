module D3.Base.Transition (transition) where

import D3.Base.Attributes (Attr)
import D3.Base.Selection (Selection(..))

import Data.Maybe (Maybe(..))

transition :: forall model. Number -> Array Attr -> Selection model 
transition duration attributes = 
  Transition { label: Nothing, duration, attributes }

