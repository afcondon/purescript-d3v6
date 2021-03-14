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
chartInit (Tuple width height) = 
  let
    origin = { x: -width / 2.0, y: -height / 2.0 }
  in
    nameSelection "General Update Pattern" $ 
      selectInDOM "div#GUP"
        []
        [ nameSelection "svg" $ 
            svg_ [ viewBox origin.x origin.y width height ]
        ]

chartUpdate :: NativeSelection -> Model Char -> Selection (Model Char) 
chartUpdate svg letters =
    extendSelection svg $ -- TODO temporary hack to type check / take stock of state of code
        Text <-+-> {
            enter:  text_ [ fill "green"
                          , y (-30.0) Px
                          , computeText (\d -> singleton (d3DOMelement d).data )]
          , update: text_ [ fill "black"
                          , y 0.0 Px ]
          , exit:   text_ [ fill "brown"
                          , y 30.0 Px ]
        }
 
