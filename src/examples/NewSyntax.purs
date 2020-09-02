module NewSyntax where

import D3.Base

import Prelude (const, identity, unit, (/))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Math (sqrt)

chargeForce = Force "charge" ForceMany
centerForce width height = Force "center" ForceCenter (width / 2) (height / 2)
centerForce' = centerForce 800 800

simulation =
  Simulation { 
      label: "simulation"
    , config: defaultConfigSimulation
    , forces: [ chargeForce, centerForce ] 
    , tick: identity
    , drag: const unit
  }

chart :: forall model. Tuple Number Number -> model -> Selection model
chart (Tuple width height) model = 
  select "div#chart" [] [
    appendNamed "svg" Svg [ staticArrayNumberAttr "viewBox" [0.0,0.0,width,height] ] 
      -- children of "svg"
      [ appendNamed "link" Group
        [ staticStringAttr "stroke" "#999", staticNumberAttr "stroke-opacity" 0.6 ] 
        (join linkEnter Nothing Nothing) 
        
      , appendNamed "node" Group
        [ staticStringAttr "stroke" "#ff", staticNumberAttr "stroke-opacity" 1.5 ]
        (join nodeEnter Nothing Nothing)
      ]
  ]

type ColorScale = Datum -> String -- TODO replace with better color, ie Web color package
scale :: ColorScale
scale _ = "red"

-- we never take a reference to the Join functions enter, update, exit so -> Unit
linkEnter = append Line [ NumberAttr "stroke-width" (\d i -> sqrt d.value)]
-- TODO note that we are not saying (\d i -> scale d.group) because contents of foreign data Datum
-- cannot be visible here, some kind of ugly coerce in the scale function, or using FFI may be required
nodeEnter = append Circle [ staticNumberAttr "r" 5.0, StringAttr "fill" (\d i -> scale d)]

-- | function to build the 
myTickFunction :: Selection -> Selection -> Array (Tuple Selection (Array Attr) )
myTickFunction link node = [
    Tuple link, [ NumberAttr "x1" (\d i -> d.source.x)
                , NumberAttr "y1" (\d i -> d.source.y)
                , NumberAttr "x2" (\d i -> d.target.x)
                , NumberAttr "y2" (\d i -> d.target.y) ]
  , Tuple node, [ NumberAttr "cx" (\d i -> d.x
 )              , NumberAttr "cy" (\d i -> d.y) ]
]

-- drag = 
--   d3Drag "node" simulation {
--       start: dragstarted
--     , drag:  dragged
--     , end:   dragended
--   }

-- attachTickFunction :: (Unit -> Unit) -> Simulation -> Unit
-- attachTickFunction tick simulation = 

