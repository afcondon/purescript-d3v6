module D3.Base.Attributes where

import D3.Base.Foreign (Datum)
import Prelude (class Show, flap, show, ($), (<>))

import Data.Array (intercalate)

-- internal definitions of Attrs, this is what the interpreter will work with
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

data DomUnit = Em | Px | Rem | Percent | NoUnit
instance showDomUnit :: Show DomUnit where
  show Em = "em"
  show Px = "px"
  show Rem = "rem"
  show Percent = "%"
  show NoUnit = ""

-- prettier definitions for attributes
strokeColor :: String -> Attr
strokeColor = StaticString "stroke"

computeStrokeColor :: (Datum -> String) -> Attr
computeStrokeColor = StringAttr "stroke"

strokeWidth :: Number -> Attr
strokeWidth = StaticNumber "stroke-width"

computeStrokeWidth :: (Datum -> Number) -> Attr
computeStrokeWidth = NumberAttr "stroke-width"
  
strokeOpacity :: Number -> Attr
strokeOpacity = StaticNumber "stroke-opacity"

computeStrokeOpacity :: (Datum -> Number) -> Attr
computeStrokeOpacity = NumberAttr "stroke-opacity"

radius :: Number -> Attr
radius = StaticNumber "r"

computeRadius :: (Datum -> Number) -> Attr
computeRadius = NumberAttr "r"

fill :: String -> Attr
fill = StaticString "fill"

computeFill :: (Datum -> String) -> Attr
computeFill = StringAttr "fill"

viewBox :: Number -> Number -> Number -> Number -> Attr
viewBox xo yo width height = StaticArrayNumber "viewBox" [ xo, yo, width, height ]

fontFamily :: String -> Attr
fontFamily = StaticString "font-family"
  
fontSize :: Number -> Attr
fontSize = StaticNumber "font-size"

computeText :: (Datum -> String) -> Attr
computeText = TextAttr

textAnchor :: String -> Attr
textAnchor = StaticString "text-anchor"

computeTextAnchor :: (Datum -> String) -> Attr
computeTextAnchor = StringAttr "text-anchor"

x :: Number -> Attr
x = StaticNumber "x"

computeX :: (Datum -> Number) -> DomUnit -> Attr
computeX f u = StringAttr "x" (\n -> show (f n) <> show u)

y :: Number -> Attr
y = StaticNumber "y"

computeY :: (Datum -> Number) -> Attr
computeY = NumberAttr "y"

dx :: String -> Attr
dx = StaticString "dx"

computeDX :: (Datum -> String) -> Attr
computeDX = StringAttr "dx"

dy :: Number -> DomUnit -> Attr
dy n u = StaticString "dy" $ show n <> show u

computeDY :: (Datum -> Number) -> DomUnit -> Attr
computeDY f u = StringAttr "dy" (\n -> show (f n) <> show u)

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

enQuote :: String -> String
enQuote string = "\"" <> string <> "\""

instance showAttribute :: Show Attr where
  show (StaticString a v) = "\n.attr(\"" <> a <> "\", \"" <> v <> "\")"
  show (StaticNumber a v) = "\n.attr(\"" <> a <> "\", \"" <> show v <> "\")"
  show (StaticArrayNumber a v) = "\n.attr(\"" <> a <> "\", \"" <> show v <> "\")"
  show (StringAttr a fn) = "\n.attr(\"" <> a <> "\", <\\d -> result>)"
  show (TextAttr fn) = "\n.attr(\"" <> "Text" <> "\", <\\d -> result>)"
  show (NumberAttr a fn) = "\n.attr(\"" <> a <> "\", <\\d -> result>)"
  show (ArrayNumberAttr a fn) = "\n.attr(\"" <> a <> "\", <\\d -> result>)"
  show (StringAttrI a fn) = "\n.attr(\"" <> a <> "\", <\\d i -> result>)"
  show (NumberAttrI a fn) = "\n.attr(\"" <> a <> "\", <\\d i -> result>)"
  show (ArrayNumberAttrI a fn) = "\n.attr(\"" <> a <> "\", <\\d i -> result>)"
