module D3.Layout.Trees (radialLink) where

import Prelude

import D3.Base.Attributes (Attr(..))
import D3.Base.Foreign (Datum)

foreign import d3LinkRadial :: (Datum -> Number) -> (Datum -> Number) -> (Datum -> String)
radialLink :: (Datum -> Number) -> (Datum -> Number) -> Attr
radialLink angleFn radius_Fn = StringAttr "d" $ d3LinkRadial angleFn radius_Fn

