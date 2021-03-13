module NewSyntax.Force (
    chart, simulation
  , Model, GraphLink, GraphNode
  , getLinks, getNodes, makeModel
  , myTickMap, myDrag
  , readModelFromFileContents) where

import D3.Base

import Affjax (Error)
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Map (fromFoldable)
import Data.Tuple (Tuple(..))
import Math (sqrt)
import Prelude (const, identity, unit, ($), (/))
import Unsafe.Coerce (unsafeCoerce)


-- this is the model used by this particular "chart" (ie force layout simulation)
type Model = { links :: Array GraphLink, nodes :: Array GraphNode }

-- minimal definition for now, need to factor in things added by simulation such as Vx, Vy
type GraphNode = SimulationNodeRow { group :: Number }
type GraphLink = { id :: ID, source :: ID, target :: ID, value :: Number }

-- | express the additions that D3 makes in terms of rows for clarity and DRY
-- after the GraphLink type has been bound in D3 it is changed to the following
type D3GraphLink = { id :: ID, source :: GraphNode, target :: GraphNode, value :: Number }

-- | definition of the force layout, resulting Selection is run by Interpreter
chart :: Tuple Number Number -> Selection Model
chart (Tuple width height) = 
  selectInDOM "div#force" "forceLayout" noAttributes $ [
    appendAs "svg" Svg [ viewBox 0.0 0.0 width height ] [
      append Group [ strokeColor "#999", strokeOpacity 0.6 ] 
        [ simpleJoin Line modelLinks $  -- "link" Selection used by name in tick function
            appendAs_ "link" Line [ strokeWidth_D (\d -> sqrt (d3Link d).value) ]
        ]
        
      , append Group [ strokeColor "#fff", strokeOpacity 1.5 ]
        [ simpleJoin Circle modelNodes $ -- "node" Selection used by name in tick function
          appendAs_ "node" Circle [ radius 5.0, fill_D colorByGroup]
        ]
      ]
  ]


-- another way of writing 'chart' above, showing how Selections can be composed
chartComposed :: Tuple Number Number -> Selection Model
chartComposed (Tuple width height) = 
  selectInDOM "div#force" "forceLayout" noAttributes $ singleton $ 
    appendAs "svg" Svg [ StaticArrayNumber "viewBox" [0.0,0.0,width,height] ] [
        append Group
          [ StaticString "stroke" "#999", StaticNumber "stroke-opacity" 0.6 ] 
          [ simpleJoin Line modelLinks $ linkEnter "link" ] -- "link" Selection used by name in tick function
        
      , append Group
          [ StaticString "stroke" "#ff", StaticNumber "stroke-opacity" 1.5 ]
          [ simpleJoin Circle modelNodes $ nodeEnter "node" ] -- "node" Selection used by name in tick function
      ]

linkEnter :: String -> Selection Model
linkEnter label = appendAs_ label Line [ NumberAttr "stroke-width" (\d -> sqrt (d3Link d).value)]

nodeEnter :: String -> Selection Model
nodeEnter label = appendAs_ label Circle [ StaticNumber "r" 5.0, StringAttr "fill" colorByGroup]


-- | definition of the particular Simulation that we are going to run
simulation :: Simulation
simulation =
  Simulation { 
      label: "simulation"
    , config: defaultConfigSimulation
    , forces: [ Force "charge" ForceMany, centerForce 800.0 900.0 ] 
    , nodes: []
    , links: []
    , tick: identity
    , drag: const unit
  }

centerForce :: Number -> Number -> Force
centerForce width height = Force "center" $ ForceCenter (width / 2.0) (height / 2.0)

-- | function to build the tick function, quite tricky
myTickMap :: TickMap Model
myTickMap = fromFoldable
  [ Tuple "link" [ NumberAttr "x1" (\d -> (unsafeCoerce d).source.x)
                 , NumberAttr "y1" (\d -> (unsafeCoerce d).source.y)
                 , NumberAttr "x2" (\d -> (unsafeCoerce d).target.x)
                 , NumberAttr "y2" (\d -> (unsafeCoerce d).target.y) ]
  , Tuple "node" [ NumberAttr "cx" (\d -> (unsafeCoerce d).x)
                 , NumberAttr "cy" (\d -> (unsafeCoerce d).y) ]
  ]

-- | utility functions and boilerplate

-- Projection functions to get subModels out of the Model for sub-selections
modelLinks :: Model -> SubModel
modelLinks model = unsafeCoerce model.links

modelNodes :: Model -> SubModel
modelNodes model = unsafeCoerce model.nodes


myDrag :: DragBehavior
myDrag = DefaultDrag "node" "simulation"

makeModel :: Array GraphLink -> Array GraphNode -> Model
makeModel links nodes = { links, nodes }

getLinks :: Model -> Array GraphLink
getLinks m = m.links

getNodes :: Model -> Array GraphNode
getNodes m = m.nodes

-- TODO the decode on the Purescript side unless files are ginormous, this is just for prototyping
foreign import readJSONJS :: String -> Model -- TODO no error handling at all here RN

readModelFromFileContents :: forall r. Either Error { body ∷ String | r } -> Model
readModelFromFileContents (Right { body } ) = readJSONJS body
readModelFromFileContents (Left err)        = { links: [], nodes: [] }


-- we give the chart our Model type but behind the scenes it is mutated by D3 and additionally
-- which projection of the "Model" is active in each Join varies so we can't have both strong
-- static type representations AND lightweight syntax with JS compatible lambdas
d3Link :: Datum -> D3GraphLink
d3Link = unsafeCoerce
d3Node :: Datum -> GraphNode
d3Node = unsafeCoerce

colorByGroup :: Datum -> String
colorByGroup d = d3SchemeCategory10JS (unsafeCoerce d :: GraphNode).group

-- drag = 
--   d3Drag "node" simulation {
--       start: dragstarted
--     , drag:  dragged
--     , end:   dragended
--   }

-- attachTickMap :: (Unit -> Unit) -> Simulation -> Unit
-- attachTickMap tick simulation = 

