module Data.Viz.Indexed where

import Data.List as L

-- || FFI for D3
foreign import data D3Element   :: Type
-- the underlying D3 selection that is passed between calls
foreign import data D3Selection :: Type
-- a Selection that's passed back in some callbacks
foreign import data Peers       :: Type
-- the `this` pointer in a callback, DOM element receiving an event
foreign import data DomElement  :: Type


data Model a = Model a

data D3Model a b = D3Model b

data Empty
data HasModel
data HasSelection
data HasDataBind
data InTransition

newtype IxDataViz i o a = IxDataViz (Model a)

-- the function that will get your result out of the IndexedMonad
runIxDataViz :: forall prev next spec. IxDataViz prev next spec -> spec
runIxDataViz (IxDataViz model) = model

instance ixMonadDataViz :: IxMonad IxDataViz where
  ipure = IxDataViz
  ibind (IxDataViz model) f = IxDataViz <<< runIxDataViz $ f model

initDataViz :: forall a. Model a -> IxDataViz Empty HasModel (Model a)
initDataViz model = IxDataViz model

data EventType = MouseEnter | MouseLeave | MouseDown | MouseUp | Click 
               | ContextMenu | DragStart | Drag | DragEnd
data Property a = StaticString String String | StaticNumber String Number
                | DynamicString String (a -> String) | DynamicNumber String (a -> Number)
                | EventHandler EventType (Event -> Unit)
type Selector   = String -- could add more validation here
type Properties = Array Property
data Select     = Select Selector Properties
type Name       = String
type Duration   = Int
data Transition = Transition Name Duration Properties
  
makeSelectionWithModel :: forall model. Select -> IxDataViz HasModel OpenSelection 

addPropertiesToSelection :: forall model. Properties -> IxDataViz OpenSelection OpenSelection

recoverSelectionFromTransition :: forall model. 

simpleModel :: L.List Int
simpleModel = L.fromFoldable [1,2,3,4]

sampleViz :: forall model. model -> IxDataViz Empty HasDataBind model
sampleViz model = initDataViz model
  :>>= makeSelectionWithModel (Select "div#chart" []) 
  :>>= addPropertiesToSelection []
  :>>= bindModelToDOM 
  :>>= addTransition (Transition "foo" 2000 [])
  :>>= addOnions
  :>>= noLettuce
  :>>= addTomato
  :>>= addTopBun
