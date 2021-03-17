module D3.Base.Attributes where

import D3.Base.Foreign (Datum)
import Data.Array (intercalate)
import Prelude (class Show, flap, show, ($), (<>))

-- internal definitions of Attrs, this is what the interpreter will work with
-- these definitions correspond exactly to what D3 allows polymorphically for attribute setters
-- the idea is to NOT make this available outside this module eventually, preferring to use
-- stronger typing and more expressive functions that wrap them, as below
-- HOWEVER it might be that there are just too many to be practical, time will tell
data Attr =
-- first the direct, static attributes
    StaticString    String String
  | StaticNumber    String Number
  | StaticArrayNumber String (Array Number)
-- then the ones that are function of Datum only
  | StringAttr      String (Datum -> String)
  | NumberAttr      String (Datum -> Number)
  | ArrayNumberAttr String (Datum -> Array Number)
-- lastly attribute functions that take Datum and the index
  | StringAttrI      String (Datum -> Number -> String)
  | NumberAttrI      String (Datum -> Number -> Number)
  | ArrayNumberAttrI String (Datum -> Number -> Array Number)
-- Text in D3 is not an attribute but syntactically and semantically it really is
-- so in our DSL we will just make it one and hide that fact
  | TextAttr        (Datum -> String)

data NumberUnit = Em | Px | Rem | Percent | Pt | NoUnit

-- prettier definitions for attributes
-- TODO we're not doing anything to check / constrain the semantics of the units here, but it would be good to do so (type-level machinery?)
-- TODO do we need more units like "1fr" etc for grids etc? 

toString :: Number -> NumberUnit -> String
toString n = 
  case _ of
    Em      -> show n <> "em"
    Px      -> show n <> "px"
    Rem     -> show n <> "rem"
    Percent -> show n <> "%"
    Pt      -> show n <> "pt"
    NoUnit  -> show n  

staticNumberAttrWithUnits :: String -> Number -> NumberUnit -> Attr
staticNumberAttrWithUnits label n u = StaticString label (toString n u)

datumToStringWithUnit :: NumberUnit -> (Datum -> Number) -> (Datum -> String)
datumToStringWithUnit u f = (\d -> toString (f d) u )

datumAndIndexToStringWithUnit :: NumberUnit -> (Datum -> Number -> Number) -> (Datum -> Number -> String)
datumAndIndexToStringWithUnit u f = (\d i -> toString (f d i) u )

-- | static versions of attribute setters, where all selected elements will be given the same value for this attribute
strokeColor :: String -> Attr
strokeColor = StaticString "stroke"

strokeOpacity :: Number -> Attr
strokeOpacity = StaticNumber "stroke-opacity"

fill :: String -> Attr
fill = StaticString "fill"

viewBox :: Number -> Number -> Number -> Number -> Attr
viewBox xo yo width height = StaticArrayNumber "viewBox" [ xo, yo, width, height ]

fontFamily :: String -> Attr
fontFamily = StaticString "font-family"
  
textAnchor :: String -> Attr
textAnchor = StaticString "text-anchor"

strokeWidth :: Number -> NumberUnit -> Attr
strokeWidth n u = staticNumberAttrWithUnits "stroke-width" n u

radius :: Number -> NumberUnit -> Attr
radius n u = staticNumberAttrWithUnits "r" n u

fontSize :: Number -> NumberUnit -> Attr
fontSize n u = staticNumberAttrWithUnits "font-size" n u

width :: Number -> NumberUnit -> Attr
width n u = staticNumberAttrWithUnits "width" n u

height :: Number -> NumberUnit -> Attr
height n u = staticNumberAttrWithUnits "height" n u

x :: Number -> NumberUnit -> Attr
x n u = staticNumberAttrWithUnits "x" n u

y :: Number -> NumberUnit -> Attr
y n u = staticNumberAttrWithUnits "y" n u

dx :: Number -> NumberUnit -> Attr
dx n u = staticNumberAttrWithUnits "dx" n u

dy :: Number -> NumberUnit -> Attr
dy n u = staticNumberAttrWithUnits "dy" n u

-- | computed versions of attribute setters, which need a function because the attribute is determined by the datum
computeStrokeColor :: (Datum -> String) -> Attr
computeStrokeColor = StringAttr "stroke"

computeFill :: (Datum -> String) -> Attr
computeFill = StringAttr "fill"

computeText :: (Datum -> String) -> Attr
computeText = TextAttr

computeTextAnchor :: (Datum -> String) -> Attr
computeTextAnchor = StringAttr "text-anchor"

computeStrokeWidth :: NumberUnit -> (Datum -> Number) -> Attr
computeStrokeWidth u f = StringAttr "stroke-width" (datumToStringWithUnit u f)
  
computeStrokeOpacity :: (Datum -> Number) -> Attr
computeStrokeOpacity = NumberAttr "stroke-opacity"

computeRadius :: NumberUnit -> (Datum -> Number) -> Attr
computeRadius u f = StringAttr "r" (datumToStringWithUnit u f)

computeX :: NumberUnit -> (Datum -> Number) -> Attr
computeX u f = StringAttr "x" (datumToStringWithUnit u f)

computeY :: NumberUnit -> (Datum -> Number) -> Attr
computeY u f = StringAttr "y" (datumToStringWithUnit u f)

computeDX :: NumberUnit -> (Datum -> Number) -> Attr
computeDX u f = StringAttr "dx" (datumToStringWithUnit u f)

computeDY :: NumberUnit -> (Datum -> Number) -> Attr
computeDY u f = StringAttr "dy" (datumToStringWithUnit u f)

-- TODO refactor all these functions that differ only in attr "string" when finished
computeXusingIndex :: NumberUnit -> (Datum -> Number -> Number) -> Attr
computeXusingIndex u f = StringAttrI "x" (datumAndIndexToStringWithUnit u f)

computeYusingIndex :: NumberUnit -> (Datum -> Number -> Number) -> Attr
computeYusingIndex u f = StringAttrI "y" (datumAndIndexToStringWithUnit u f)

data LineJoin = Arcs | Bevel | Miter | MiterClip | Round
instance showLineJoin :: Show LineJoin where
  show Arcs = "arcs"
  show Bevel = "bevel"
  show Miter = "miter"
  show MiterClip = "miter-clip"
  show Round = "round"

strokeLineJoin :: LineJoin -> Attr
strokeLineJoin linejoin = StaticString "stroke-linejoin" $ show linejoin
    
type TransformFn = Datum -> String

transform :: Array TransformFn -> Attr
transform fs = StringAttr "transform" (\d -> showTransform d)
  where
    showTransform d = intercalate " " $ flap fs d

-- |              Show instance etc

bracket :: Array String -> String
bracket ss = "(" <> 
  (intercalate "," ss)
  <> ")"

prefix :: String -> String
prefix s = "\n.attr" <> s

enQuote :: String -> String
enQuote string = "\"" <> string <> "\""

instance showAttribute :: Show Attr where
  show (StaticString a v)      = prefix $ bracket $ [ enQuote a,  enQuote v]
  show (StaticNumber a v)      = prefix $ bracket $ [ enQuote a, enQuote $ show v ]
  show (StaticArrayNumber a v) = prefix $ bracket $ [ enQuote a, enQuote $ show v ] 
  show (TextAttr fn)           = prefix $ bracket $ [ enQuote "Text", "<\\d -> result>" ]
  show (StringAttr a fn)       = prefix $ bracket $ [ enQuote a, "<\\d -> result>" ]
  show (NumberAttr a fn)       = prefix $ bracket $ [ enQuote a, "<\\d -> result>" ]
  show (ArrayNumberAttr a fn)  = prefix $ bracket $ [ enQuote a, "<\\d -> result>" ]
  show (StringAttrI a fn)      = prefix $ bracket $ [ enQuote a, "<\\d i -> result>" ]
  show (NumberAttrI a fn)      = prefix $ bracket $ [ enQuote a, "<\\d i -> result>" ]
  show (ArrayNumberAttrI a fn) = prefix $ bracket $ [ enQuote a, "<\\d i -> result>" ]
