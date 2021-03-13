module NewSyntax.GUP where

import D3.Base

import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple(..))
import Prelude (identity, negate, unit, (-), (/), ($))
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
        [ nameSelection "svg" $ svg_ [ viewBox origin.x origin.y width height ]
        ]

chartUpdate :: NativeSelection -> Model Char -> Selection (Model Char) 
chartUpdate svg letters =
    extendSelection (unsafeCoerce svg) $ -- TODO temporary hack to type check / take stock of state of code
        join Text identity' {
            enter:  text_ [ fill "green", StaticNumber "y" (-30.0), computeText (\d -> singleton (d3DOMelement d).data )]
          , update: text_ [ fill "black", StaticNumber "y" 0.0 ]
          , exit:   text_ [ fill "brown", StaticNumber "y" 30.0 ]
        }
 
-- TODO this seems like a heinous wart, we need a (Model -> SubModel) even if it's functionally `identity` 
-- this should be fixed in Base or Interpreter 
identity' :: forall a. Model a -> SubModel
identity' = unsafeCoerce