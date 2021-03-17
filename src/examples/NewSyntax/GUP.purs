module NewSyntax.GUP where

import D3.Base
import Data.Tuple (Tuple(..))
import Prelude (negate, ($), (*), (/))
import Unsafe.Coerce (unsafeCoerce)

-- with such simple data and simple data viz, the data in the DOM will be exactly same as Model data
-- this means that a simple coerce suffices to turn Datum -> D3DOMelement and also that no Projection
-- function is required, hence the use of join_ 
type D3DOMelement = Char

type Model a = Array a

chartInit :: Tuple Number Number -> Selection (Model Char)
chartInit (Tuple width height) = do
  let origin = { x: -width / 2.0, y: -height / 2.0 }

  selectInDOM "div#GUP" [] [-- this is an InitialSelect
    svg 
      [ viewBox origin.x origin.y width height ]
      [ join_ "gup" Text $
        enterUpdateExit {
          enter: withAttributes [ fill "green"
                                , y (-30.0) Px
                                , computeXusingIndex Px (\d i -> i * 10.0 )
                                , computeText (\d -> unsafeCoerce d )] -- we know the foreign Datum is a Char

        , update: withAttributes [ fill "black",  y 0.0 Px ]
          
        , exit:   withAttributes [ fill "brown", y 30.0 Px ]
        }
      ]    
  ] 
