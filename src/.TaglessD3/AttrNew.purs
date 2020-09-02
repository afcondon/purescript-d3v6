module TaglessD3.AttrNew (
      Attr(..)
    , Attr'
    , AttrSetter
    , D3Attr(..)
    , attrValue
    , attrFunction
    , attributes
    , showListOfAttributes
    , getTag
    , CssUnit(..)
    )where


import D3.Base (D3Element)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (Fn4)
import Data.List (List(..), foldl, fromFoldable, intercalate)
import Prelude (class Show, const, map, show, ($), (<>))

-- The supported length unit identifiers are: em, ex, px, pt, pc, cm, mm, in, and percentages.
data CssUnit = Em | Ex | Px | Pt | Pc | Cm | Mm | In | Percent | NoUnit
instance showCssUnit :: Show CssUnit where
    show Em = "em"
    show Ex = "ex"
    show Px = "px"
    show Pt = "pt"
    show Pc = "pc"
    show Cm = "cm"
    show Mm = "mm"
    show In = "in"
    show Percent  = "%"
    show NoUnit = "" -- well, i don't love this, probably an argument for making styles differently TODO

type AttrSetter d v = d -> Number -> (Array D3Element) -> D3Element -> v

data D3Attr a = D3Attr { value :: a
                       , showValue :: a -> String -- this only produces a String, not enough for actually passing to D3
                       , units :: String
                    }
type Attr' = Exists D3Attr

data Attr = CX                  Attr' -- circles only
          | CY                  Attr' -- circles only
          | R                   Attr' -- circles only
          | X                   Attr'
          | Y                   Attr'
          | DX                  Attr' -- string because it might have units with
          | DY                  Attr'
          | Height              Attr'
          | Width               Attr'
          | StrokeWidth         Attr'
          | StrokeOpacity       Attr'
          | FillOpacity         Attr'
          | Opacity             Attr'
          | D                   Attr' -- has to be an SVGPathString
          | Id                  Attr'
          | StrokeLineCap       Attr' -- Round | Butt | Square
          | PatternUnits        Attr' -- userSpaceOnUse | objectBoundingBox
          | Text                Attr'
          | Type                Attr' -- images only
          | Style        String Attr'
          | Class               Attr'
          | Property     String Attr'
        --   | Fill                (ValueOrCallback d Color)
        --   | Stroke              (ValueOrCallback d Color)

showListOfAttributes :: List Attr -> String
showListOfAttributes attrs = intercalate ", " $ foldl go Nil attrs
    where
    go :: List String -> Attr -> List String
    go acc attr = Cons (show attr) acc

showAll :: List Attr' -> List String
showAll = map (runExists showOne)

showOne :: ∀ a. D3Attr a -> String
showOne (D3Attr ops) = ops.showValue ops.value

instance showD3Attr :: Show (D3Attr a) where
  show (D3Attr { value, showValue, units }) = "Attr a: " <> showValue value <> show units

instance showAttr :: Show Attr where
  show (CX a')
    =  "CX: " <> runExists showOne a' -- showOne :: ∀ a. D3Attr a -> String
  show (CY a')
    =  "CY: " <> runExists showOne a'
  show (R a')
    =  "R: " <> runExists showOne a'
  show (X a')
    =  "X: " <> runExists showOne a'
  show (Y a')
    =  "Y: " <> runExists showOne a'
  show (DX a')
    =  "DX: " <> runExists showOne a'
  show (DY a')
    =  "DY: " <> runExists showOne a'
  show (Height a')
    =  "Height: " <> runExists showOne a'
  show (Width a')
    =  "Width: " <> runExists showOne a'
  show (StrokeWidth a')
    =  "StrokeWidth: " <> runExists showOne a'
  show (StrokeOpacity a')
    =  "StrokeOpacity: " <> runExists showOne a'
  show (FillOpacity a')
    =  "FillOpacity: " <> runExists showOne a'
  show (Opacity a')
    =  "Opacity: " <> runExists showOne a'
  show (D a')
    =  "D: " <> runExists showOne a'
  show (Id a')
    =  "Id: " <> runExists showOne a'
  show (StrokeLineCap a')
    =  "StrokeLineCap: " <> runExists showOne a'
  show (PatternUnits a')
    =  "PatternUnits: " <> runExists showOne a'
  show (Text a')
    =  "Text: " <> runExists showOne a'
  show (Type a')
    =  "Type: " <> runExists showOne a'
  show (Style s a')
    =  "Style: " <> s <> " " <> runExists showOne a'
  show (Class a')
    =  "Class: " <> runExists showOne a'
  show (Property s a')
    =  "Property: " <> s <> " " <> runExists showOne a'

attributes :: ∀ f a. (Foldable f) => f a -> List a
attributes = fromFoldable

-- constructors for attribute values, both values and callback functions
attrValue :: ∀ v. Show v => v -> CssUnit -> Attr'
attrValue v NoUnit = mkExists (D3Attr { value: v, showValue: show, units: "Simple String Value" })
attrValue v u      = mkExists (D3Attr { value: show v <> show u, showValue: show, units: show u })

attrFunction :: ∀ d v. AttrSetter d v -> CssUnit -> Attr'
attrFunction f u = mkExists (D3Attr { value: uncurryAttrSetter (show u) f, showValue: const "(function)", units: show u })

foreign import uncurryAttrSetter :: forall d v. String -> AttrSetter d v -> Fn4 d Number (Array D3Element) D3Element String

getTag :: Attr -> String
getTag a =
    case a of
    (CX _)            -> "cx"
    (CY _)            -> "cy"
    (R _)             -> "r"
    (X _)             -> "x"
    (Y _)             -> "y"
    (DX _)            -> "dx"
    (DY _)            -> "dy"
    (Height _)        -> "height"
    (Width _)         -> "width"
    (StrokeWidth _)   -> "strokewidth"
    (StrokeOpacity _) -> "strokeopacity"
    (FillOpacity _)   -> "fillopacity"
    (Opacity _)       -> "opacity"
    (D _)             -> "d"
    (Id _)            -> "id"
    (StrokeLineCap _) -> "strokelinecap"
    (PatternUnits _)  -> "patternunits"
    (Text _)          -> "text"
    (Type _)          -> "type"
    (Style _ _)       -> "style"
    (Class _)         -> "class"
    (Property _ _)    -> "property"
