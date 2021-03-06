module D3.Interpreter.Layouts.Simulation where

import D3.Interpreter.Foreign (NativeJS, addAttrFnToTickJS, attachDefaultDragBehaviorJS, attachTickFnToSimulationJS, forceCenterJS, forceCollideJS, forceManyJS, forceRadialJS, forceXJS, forceYJS, initSimulationJS, putLinksInSimulationJS, putNodesInSimulationJS, startSimulationJS, stopSimulationJS)
import Prelude (Unit, bind, discard, flip, pure, unit, ($), (<$>))

import Control.Monad.State (get, put)
import D3.Base (Attr, DragBehavior(..), Force(..), NativeSelection, Simulation(..), TickMap)
import D3.Base.Layout.Simulation (ForceType(..))
import D3.Interpreter.Types (D3, D3State(..))
import Data.Array (concatMap, foldl, fromFoldable, (:))
import Data.Bifunctor (lmap)
import Data.Map (Map, insert, lookup, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)

-- |                   FORCE LAYOUT (SIMULATION) interpreter
startSimulation :: forall model. NativeSelection -> D3 model Unit
startSimulation simulation = pure $ startSimulationJS simulation

stopSimulation :: forall model. NativeSelection -> D3 model Unit
stopSimulation simulation = pure $ stopSimulationJS simulation

-- | get a reference to a simulation that we can then use elsewhere
-- | ie having interpreted a Selection such that the DOM is set up to run a simulation
-- | NB this is actually modifying the model in the Context
interpretSimulation :: forall model node link. Simulation -> 
                              (model -> Array node) ->  -- project nodes from model
                              (model -> Array link) ->  -- project links from model
                              (Array link -> Array node -> model) -> -- repackage nodes & links as model
                              D3 model NativeSelection
interpretSimulation (Simulation r) getNodes getLinks repackage =
  do
    (Context model scope) <- get
    let sim = initSimulationJS r.config
        nodes = nativeNodes $ getNodes model
        links = nativeLinks $ getLinks model
    -- updateScope sim (Just r.label)
    traverse_ (interpretForce sim) r.forces
    let initializedNodes = unnativeNodes $ putNodesInSimulationJS sim nodes
        initializedLinks = unnativeLinks $ putLinksInSimulationJS sim links
        initializedModel = repackage initializedLinks initializedNodes
    put (Context initializedModel (insert r.label sim scope))
    pure sim
  where
    nativeNodes   = unsafeCoerce :: Array node -> Array NativeJS
    nativeLinks   = unsafeCoerce :: Array link -> Array NativeJS
    unnativeNodes = unsafeCoerce :: Array NativeJS -> Array node
    unnativeLinks = unsafeCoerce :: Array NativeJS -> Array link

interpretForce :: forall model. NativeSelection -> Force -> D3 model Unit
interpretForce simulation = do
  case _ of
    (Force label ForceMany)                 -> pure $ forceManyJS simulation label 
    (Force label (ForceCenter cx cy))       -> pure $ forceCenterJS simulation label cx cy
    -- (Force (ForceLink links idFn)) -> pure $ forceLinks
    (Force label (ForceCollide radius_))     -> pure $ forceCollideJS simulation label radius_
    (Force label (ForceX x))                -> pure $ forceXJS simulation label x
    (Force label (ForceY y))                -> pure $ forceYJS simulation label y
    (Force label (ForceRadial cx cy))       -> pure $ forceRadialJS simulation label cx cy
    (Force label Custom)                    -> pure $ unit -- do this later as needed

-- getNativeSelections :: Map String NativeSelection -> TickMap model -> Array (Tuple NativeSelection (Array (Tuple attr fn)))
-- getNativeSelections scope []


getNativeSelection :: forall x. (Map String NativeSelection) -> Map String x -> Array (Tuple NativeSelection x)
getNativeSelection scopeMap tickMap = fromFoldable nativeTuples
  where
    tickTuples :: Array (Tuple String x)
    tickTuples = toUnfoldable tickMap

    maybeNativeTuple :: Tuple String x -> Tuple (Maybe NativeSelection) x
    maybeNativeTuple = lmap (flip lookup scopeMap)

    maybeNativeTuples :: Array (Tuple (Maybe NativeSelection) x)
    maybeNativeTuples = maybeNativeTuple <$> tickTuples

    nativeTuples :: Array (Tuple NativeSelection x)
    nativeTuples = foldl foldFn [] maybeNativeTuples 

    foldFn list (Tuple (Just a) b) = (Tuple a b) : list
    foldFn list (Tuple Nothing _)  = list

addAttrFnToTick :: forall model. Tuple NativeSelection Attr -> D3 model Unit
addAttrFnToTick (Tuple selection attr) = pure $ addAttrFnToTickJS selection attr 

interpretTickMap :: forall model. NativeSelection -> TickMap model -> D3 model Unit
interpretTickMap simulation tickMap = do
  (Context model scope) <- get
  --  [(label,attrs)] -> [(nativeselection, attr)] so as enable build up of list on JS side
  let 
    attrs :: Array (Tuple NativeSelection Attr)
    attrs = concatMap (\(Tuple x ys) -> (Tuple x) <$> ys) (getNativeSelection scope tickMap)
  -- TODO pending better solution will pass Attr (purescript type) over FFI and decode there
  traverse_ addAttrFnToTick attrs
  pure $ attachTickFnToSimulationJS simulation

interpretDrag :: forall model. DragBehavior -> D3 model Unit 
interpretDrag (DefaultDrag selectionName simulationName) = do
  (Context model scope) <- get
  let selection  = lookup selectionName scope
  let simulation = lookup simulationName scope
  pure $ case selection, simulation of
          (Just sel), (Just sim) -> attachDefaultDragBehaviorJS sel sim
          _, _ -> unit


