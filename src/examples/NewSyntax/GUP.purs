module NewSyntax.GUP where

import D3.Base (Element(..), NumberUnit(..), Selection, computeText, enter, fill, join_, selectInDOM, svg, viewBox, withAttributes, y)
import Prelude (negate, ($), (/))

import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)


type D3DOMelement = Char

type Model a = Array a

chartInit :: Tuple Number Number -> Selection (Model Char)
chartInit (Tuple width height) = do
  let origin = { x: -width / 2.0, y: -height / 2.0 }

  selectInDOM "div#GUP" [] [-- this is an InitialSelect
    svg [ viewBox origin.x origin.y width height ]
      [ join_ "gup" Text $
        enter $ withAttributes [ fill "green", y (-30.0) Px
                               , computeText (\d -> unsafeCoerce d )] -- we know the foreign Datum is a Char
        -- enterUpdateExit {
        --   enter: withAttributes [ fill "green",   y (-30.0) Px
        --                         , computeText (\d -> singleton (d3DOMelement d).data )]

        -- , update: withAttributes [ fill "black",  y 0.0 Px ]
          
        -- , exit:   withAttributes [ fill "brown" , y 30.0 Px ]
        -- }
      ]    
  ] 
