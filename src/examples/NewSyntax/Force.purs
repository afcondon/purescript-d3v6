module NewSyntax.Force (
    chart, simulation
  , Model, GraphLink, GraphNode
  , getLinks, getNodes, makeModel
  , myTickMap
  , readModelFromFileContents) where

import D3.Base

import Affjax (Error)
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Math (sqrt)
import Prelude (const, identity, unit, ($), (/))
import Unsafe.Coerce (unsafeCoerce)


-- this is the model that this particular "chart" / simulation uses
type Model = { links :: Array GraphLink, nodes :: Array GraphNode }

makeModel :: Array GraphLink -> Array GraphNode -> Model
makeModel links nodes = { links, nodes }

getLinks :: Model -> Array GraphLink
getLinks m = m.links

getNodes :: Model -> Array GraphNode
getNodes m = m.nodes

-- minimal definition for now, need to factor in things added by simulation such as Vx, Vy
type GraphNode = { x :: Number, y :: Number, group :: Number }
type GraphLink = { id :: ID, source :: ID, target :: ID, value :: Number }

-- after the GraphLink type has been bound in D3 it is changed to the following
type D3GraphNode = { x :: Number, y :: Number, group :: Number, vx :: Number, vy :: Number, index :: Number }
type D3GraphLink = { id :: ID, source :: D3GraphNode, target :: D3GraphNode, value :: Number }

-- do the decode on the Purescript side unless files are ginormous, this is just for prototyping
foreign import readJSONJS :: String -> Model -- TODO no error handling at all here RN

readModelFromFileContents :: forall r. Either Error { body ∷ String | r } -> Model
readModelFromFileContents (Right { body } ) = readJSONJS body
readModelFromFileContents (Left err)        = { links: [], nodes: [] }


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
    , nodes: []
    , links: []
    , tick: identity
    , drag: const unit
  }

-- we give the chart our Model type but behind the scenes it is mutated by D3 and additionally
-- which projection of the "Model" is active in each Join varies so we can't have both strong
-- static type representations AND lightweight syntax with JS compatible lambdas
d3Link :: Datum -> D3GraphLink
d3Link = unsafeCoerce
d3Node :: Datum -> D3GraphNode
d3Node = unsafeCoerce

chart :: Tuple Number Number -> Selection Model
chart (Tuple width height) = 
  initialSelect "div#force" "forceLayout" [] $ [
    appendNamed "svg" Svg [ StaticArrayNumber "viewBox" [0.0, 0.0,width,height] ] [
      append Group
        [ StaticString "stroke" "#999", StaticNumber "stroke-opacity" 0.6 ] 
        [ join "joinlink" modelLinks Line
          (appendNamed "link" Line [ NumberAttr "stroke-width" (\d -> sqrt (d3Link d).value)] [])
          noUpdate noExit ]
        
      , append Group
        [ StaticString "stroke" "#ff", StaticNumber "stroke-opacity" 1.5 ]
        [ join "joinnode" modelNodes Circle
          (appendNamed "node" Circle [ StaticNumber "r" 5.0, StringAttr "fill" (\d -> scale d)] [])
          noUpdate noExit ]
      ]
  ]

type ColorScale = Datum -> String -- TODO replace with better color, ie Web color package
scale :: ColorScale
scale _ = "red"

-- Projection functions to get subModels out of the Model for sub-selections
modelLinks :: Model -> SubModel
modelLinks model = unsafeCoerce model.links

modelNodes :: Model -> SubModel
modelNodes model = unsafeCoerce model.nodes

-- another version of the 'chart' above, showing how Selections can be composed
chartComposed :: Tuple Number Number -> Selection Model
chartComposed (Tuple width height) = 
  initialSelect "div#force" "forceLayout" [] $ singleton $ 
    appendNamed "svg" Svg [ StaticArrayNumber "viewBox" [0.0,0.0,width,height] ] [
      append Group
        [ StaticString "stroke" "#999", StaticNumber "stroke-opacity" 0.6 ] 
        [ join "link" modelLinks Line
          linkEnter
          noUpdate noExit ]
        
      , append Group
        [ StaticString "stroke" "#ff", StaticNumber "stroke-opacity" 1.5 ]
        [ join "node" modelNodes Circle
          nodeEnter
          noUpdate noExit ]
      ]

linkEnter :: Selection Model
linkEnter = append Line [ NumberAttr "stroke-width" (\d -> sqrt (d3Link d).value)] []

-- TODO note that we are not saying (\d i -> scale d.group) because contents of foreign data Datum
-- cannot be visible here, some kind of ugly coerce in the scale function, or using FFI may be required
nodeEnter :: Selection Model
nodeEnter = append Circle [ StaticNumber "r" 5.0, StringAttr "fill" (\d -> scale d)] []

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

-- drag = 
--   d3Drag "node" simulation {
--       start: dragstarted
--     , drag:  dragged
--     , end:   dragended
--   }

-- attachTickMap :: (Unit -> Unit) -> Simulation -> Unit
-- attachTickMap tick simulation = 

