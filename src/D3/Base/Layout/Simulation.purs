module D3.Base.Layout.Simulation where

import D3.Base.Attributes (Attr)
import D3.Base.Selection (Label)
import Prelude (Unit)

import Data.Map (Map)

-- | a record to initialize / configure simulations
type SimulationConfig = { 
      alpha         :: Number
    , alphaTarget   :: Number
    , alphaMin      :: Number
    , alphaDecay    :: Number
    , velocityDecay :: Number
}

defaultConfigSimulation :: SimulationConfig
defaultConfigSimulation = { 
      alpha        : 1.0
    , alphaTarget  : 0.0
    , alphaMin     : 0.0001
    , alphaDecay   : 0.0228
    , velocityDecay: 0.4
}

-- this is the row that gets added ot your Model's nodes when initialized by D3
type SimulationNodeRow :: forall k. k -> Type
type SimulationNodeRow r = { x :: Number, y :: Number, group :: Number, vx :: Number, vy :: Number, index :: Number }

-- | Force Layout core types
type ID = Int -- TODO this needs to be polymorphic eventually
type Link = forall r. { id :: ID, source :: ID, target :: ID | r }
type Node = forall r i. { id :: i | r }
type IdFn = Link -> ID
data Force = Force Label ForceType
data ForceType =
    ForceMany
  | ForceCenter Number Number
  -- | ForceLink (Array Link) IdFn
  | ForceCollide Number
  | ForceX Number
  | ForceY Number
  | ForceRadial Number Number
  | Custom

type SimulationRecord = { 
    label  :: String
  , config :: SimulationConfig
  , nodes  :: Array Node
  , links  :: Array Link
  , forces :: Array Force
  , tick   :: Unit -> Unit -- could be Effect Unit
  , drag   :: Simulation -> Unit -- could be Effect Unit
}

data Simulation = Simulation SimulationRecord
type TickMap :: forall k. k -> Type
type TickMap model = Map String (Array Attr)
data DragBehavior = DefaultDrag String String -- only one implementation rn and implemented on JS side 
