module D3.Base
  ( D3Element
  , D3Selection
  , Peers
  , DomElement
  , D3Typenames
  , ListenerType(..)
  , Typenames(..)
  , Filter(..)
  , Point
  , Hierarchy
  , Index
  , Nodes
  , transparent
  , opaque
  ) where

import Effect
import Data.Array ((:))
import Data.Foldable (foldr, intercalate)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype)
import Prelude (class Show, show, ($), (<>))

-- || FFI for D3
foreign import data D3Element :: Type
-- the underlying D3 selection that is passed between calls
foreign import data D3Selection :: Type
-- a Selection that's passed back in some callbacks
foreign import data Peers       :: Type
-- the `this` pointer in a callback, DOM element receiving an event
foreign import data DomElement  :: Type

newtype Hierarchy d = Hierarchy { name :: String, children :: Array (Hierarchy d), datum :: d }

derive instance newtypeHierarchy :: Newtype (Hierarchy d) _

type Index       = Number
type D3Typenames = String
type Nodes       = Array D3Element
type Point       = { x :: Number, y :: Number }

-- | types for drags and zooms
data ListenerType = StartDrag | Drag | EndDrag
                  | StartZoom | Zoom | EndZoom

instance isShowListenerType :: Show ListenerType where
  show StartDrag = "start"
  show Drag      = "drag"
  show EndDrag   = "end"
  show StartZoom = "start"
  show Zoom      = "zoom"
  show EndZoom   = "end"

data Typenames = TypeNames (Array { name :: Maybe String, type :: ListenerType })

-- smush the Typenames down to a single string which D3 will (wastefully) parse out again)
instance isShowTypenames :: Show Typenames where
  show (TypeNames s) = intercalate " " $ foldr f [] s
    where
      f {name: (Just n), type: t } acc = ((show t) <> "." <> n) : acc
      f {name: Nothing,  type: t } acc =             (show t)  : acc

foreign import transparent :: String
foreign import opaque :: String

data Filter d       = Selector  String
                    | Predicate (d -> Boolean)
