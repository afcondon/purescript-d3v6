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

data NumberWithUnit = Em Number | Px Number | Rem Number | Percent Number | Pt Number | NoUnit Number

-- prettier definitions for attributes
-- TODO we're not doing anything to check / constrain the semantics of the units here, but it would be good to do so (type-level machinery?)
-- TODO do we need more units like "1fr" etc for grids etc? 

-- | Note that in practice any number that has units is really encoding as a string, but that's how JavaScript do
instance showNumberWithUnit :: Show NumberWithUnit where
  show = 
    case _ of
      (Em n)      -> show n <> "em"
      (Px n)      -> show n <> "px"
      (Rem n)     -> show n <> "rem"
      (Percent n) -> show n <> "%"
      (Pt n)      -> show n <> "pt"
      (NoUnit n)  -> show n  

staticNumberAttrWithUnits :: String -> NumberWithUnit -> Attr
staticNumberAttrWithUnits label n = StaticString label (show n)

datumToStringWithUnit :: (Datum -> NumberWithUnit) -> (Datum -> String)
datumToStringWithUnit f = (\d -> show $ f d )

-- | static versions of attribute setters, where all selected elements will be given the same value for this attribute
strokeColor :: String -> Attr
strokeColor = StaticString "stroke"

strokeWidth :: NumberWithUnit -> Attr
strokeWidth = staticNumberAttrWithUnits "stroke-width"

strokeOpacity :: Number -> Attr
strokeOpacity = StaticNumber "stroke-opacity"

radius :: NumberWithUnit -> Attr
radius = staticNumberAttrWithUnits "r"

fill :: String -> Attr
fill = StaticString "fill"

viewBox :: Number -> Number -> Number -> Number -> Attr
viewBox xo yo width height = StaticArrayNumber "viewBox" [ xo, yo, width, height ]

fontFamily :: String -> Attr
fontFamily = StaticString "font-family"
  
textAnchor :: String -> Attr
textAnchor = StaticString "text-anchor"

fontSize :: NumberWithUnit -> Attr
fontSize = staticNumberAttrWithUnits "font-size"

width :: NumberWithUnit -> Attr
width = staticNumberAttrWithUnits "width"

height :: NumberWithUnit -> Attr
height = staticNumberAttrWithUnits "height"

x :: NumberWithUnit -> Attr
x = staticNumberAttrWithUnits "x"

y :: NumberWithUnit -> Attr
y = staticNumberAttrWithUnits "y"

dx :: NumberWithUnit -> Attr
dx = staticNumberAttrWithUnits "dx"

dy :: NumberWithUnit -> Attr
dy = staticNumberAttrWithUnits "dy"

-- | computed versions of attribute setters, which need a function because the attribute is determined by the datum
computeStrokeColor :: (Datum -> String) -> Attr
computeStrokeColor = StringAttr "stroke"

computeFill :: (Datum -> String) -> Attr
computeFill = StringAttr "fill"

computeText :: (Datum -> String) -> Attr
computeText = TextAttr

computeTextAnchor :: (Datum -> String) -> Attr
computeTextAnchor = StringAttr "text-anchor"

computeStrokeWidth :: (Datum -> NumberWithUnit) -> Attr
computeStrokeWidth f = StringAttr "stroke-width" (datumToStringWithUnit f)
  
computeStrokeOpacity :: (Datum -> Number) -> Attr
computeStrokeOpacity = NumberAttr "stroke-opacity"

computeRadius :: (Datum -> NumberWithUnit) -> Attr
computeRadius f = StringAttr "r" (datumToStringWithUnit f)

computeX :: (Datum -> NumberWithUnit) -> Attr
computeX f = StringAttr "x" (datumToStringWithUnit f)

computeY :: (Datum -> NumberWithUnit) -> Attr
computeY f = StringAttr "y" (datumToStringWithUnit f)

computeDX :: (Datum -> NumberWithUnit) -> Attr
computeDX f = StringAttr "dx" (datumToStringWithUnit f)

computeDY :: (Datum -> NumberWithUnit) -> Attr
computeDY f = StringAttr "dy" (datumToStringWithUnit f)

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
