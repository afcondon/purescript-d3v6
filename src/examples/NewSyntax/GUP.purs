module NewSyntax.GUP where

import D3.Base

import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple(..))
import Prelude (negate, ($), (/))
import Unsafe.Coerce (unsafeCoerce)


type D3DOMelement = {
    "data"   :: Char
  , x        :: Number
  , y        :: Number
}

d3DOMelement :: Datum -> D3DOMelement
d3DOMelement = unsafeCoerce

type Model a = { 
      letters :: Array a
}

makeModel :: Array Char -> Model Char
makeModel letters = { letters }

chartInit :: Tuple Number Number -> Selection (Model Char)
chartInit (Tuple width height) = do
  let origin = { x: -width / 2.0, y: -height / 2.0 }
  nameSelection "General Update Pattern" $ 
    selectInDOM "div#GUP" -- this is an InitialSelect
      []
      [ nameSelection "svg" $ 
          svg_ [ viewBox origin.x origin.y width height ]
      ]

-- so, the name of the chart is hard-wired here but obviously could be a
-- parameter in the 
chartUpdate :: Model Char -> Selection (Model Char) 
chartUpdate letters =
    modifySelection "General Update Pattern" $
        Text <-+-> {
            enter:  text_ [ fill "green"
                          , y $ Px (-30.0)
                          , computeText (\d -> singleton (d3DOMelement d).data )]
          , update: text_ [ fill "black"
                          , y $ Px 0.0 ]
          , exit:   text_ [ fill "brown"
                          , y $ Px 30.0 ]
        }
 
