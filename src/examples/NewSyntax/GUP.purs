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
    initialSelect "div#GUP" "General Update Pattern" noAttrs [
        appendNamed "svg" Svg [ viewBox origin.x origin.y width height ] noChildren
    ]

chartUpdate :: Selection (Model Char) -> Model Char -> Selection (Model Char) 
chartUpdate svg letters =
    extendSelection svg $
        join Text identity' 
            (append Text [ fill "green"
                        , StaticNumber "y" (-30.0)
                        , TextAttr (\d -> singleton (d3DOMelement d).data )] noChildren)
            (append Text [ fill "black"
                        , StaticNumber "y" 0.0 ] noChildren)
            (append Text [ fill "brown"
                        , StaticNumber "y" 30.0 ] noChildren)
 
-- TODO this seems like a heinous wart, we need a (Model -> SubModel) even if it's functionally `identity` 
-- this should be fixed in Base or Interpreter 
identity' :: forall a. Model a -> SubModel
identity' = unsafeCoerce