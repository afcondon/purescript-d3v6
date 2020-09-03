module NewSyntax where

import D3.Base

import Data.Array (singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Math (sqrt)
import Prelude (Unit, const, identity, mempty, unit, ($), (/))
import Unsafe.Coerce (unsafeCoerce)

chargeForce :: Force
chargeForce = Force "charge" ForceMany
centerForce :: Number -> Number -> Force
centerForce width height = Force "center" $ ForceCenter (width / 2.0) (height / 2.0)

simulation :: Simulation
simulation =
  Simulation { 
      label: "simulation"
    , config: defaultConfigSimulation
    , forces: [ chargeForce, centerForce 800.0 900.0 ] 
    , tick: identity
    , drag: const unit
  }

-- minimal definition for now, need to factor in things added by simulation such as Vx, Vy
type GraphNode = { x :: Number, y :: Number, group :: Number }
type GraphLink = { id :: ID, source :: ID, target :: ID, value :: Number }

-- after the GraphLink type has been bound in D3 it is changed to the following
type D3GraphNode = { x :: Number, y :: Number, group :: Number, vx :: Number, vy :: Number, index :: Number }
type D3GraphLink = { id :: ID, source :: D3GraphNode, target :: D3GraphNode, value :: Number }

-- we give the chart our Model type but behind the scenes it is mutated by D3 and additionally
-- which projection of the "Model" is active in each Join varies so we can't have both strong
-- static type representations AND lightweight syntax with JS compatible lambdas
type Model = { links :: Array GraphLink, nodes :: Array GraphNode }
castLink :: Datum -> D3GraphLink
castLink = unsafeCoerce
castNode :: Datum -> D3GraphNode
castNode = unsafeCoerce

chart :: Tuple Number Number -> Model -> Selection Model
chart (Tuple width height) model =
  let
    -- modelLinks :: Model -> SubModel
    modelLinks m = unsafeCoerce $ m.links
    -- modelNodes :: Model -> SubModel
    modelNodes m = unsafeCoerce $ m.nodes
  in 
    select "div#chart" [] $ singleton $ 
      appendNamed "svg" Svg [ staticArrayNumberAttr "viewBox" [0.0,0.0,width,height] ] [
        appendNamed "link" Group
          [ staticStringAttr "stroke" "#999", staticNumberAttr "stroke-opacity" 0.6 ] 
          [join modelLinks linkEnter Nothing Nothing]
          
        , appendNamed "node" Group
          [ staticStringAttr "stroke" "#ff", staticNumberAttr "stroke-opacity" 1.5 ]
          [join modelNodes nodeEnter Nothing Nothing]
        ]

type ColorScale = Datum -> String -- TODO replace with better color, ie Web color package
scale :: ColorScale
scale _ = "red"

-- we never take a reference to the Join functions enter, update, exit so -> Unit
linkEnter :: Selection Model -> Unit
linkEnter = mempty $ append Line [ NumberAttr "stroke-width" (\d i -> sqrt (castLink d).value)]
-- TODO note that we are not saying (\d i -> scale d.group) because contents of foreign data Datum
-- cannot be visible here, some kind of ugly coerce in the scale function, or using FFI may be required
nodeEnter :: Selection Model -> Unit
nodeEnter = mempty $ append Circle [ staticNumberAttr "r" 5.0, StringAttr "fill" (\d i -> scale d)]

-- | function to build the tick function, quite tricky
myTickFunction :: Selection Model-> Selection Model -> Array (Tuple (Selection Model) (Array Attr) )
myTickFunction link node = [
    Tuple link [ NumberAttr "x1" (\d i -> (castLink d).source.x)
                , NumberAttr "y1" (\d i -> (castLink d).source.y)
                , NumberAttr "x2" (\d i -> (castLink d).target.x)
                , NumberAttr "y2" (\d i -> (castLink d).target.y) ]
  , Tuple node [ NumberAttr "cx" (\d i -> (castNode d).x
 )              , NumberAttr "cy" (\d i -> (castNode d).y) ]
]

-- drag = 
--   d3Drag "node" simulation {
--       start: dragstarted
--     , drag:  dragged
--     , end:   dragended
--   }

-- attachTickFunction :: (Unit -> Unit) -> Simulation -> Unit
-- attachTickFunction tick simulation = 

