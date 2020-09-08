module D3.Interpreter where

import Prelude

import Control.Monad.State (StateT(..), get, modify_, put)
import D3.Base (Attr(..), Force(..), ForceType(..), NativeSelection, Selection(..), Simulation)
import Data.Foldable (foldl, traverse_)
import Data.Map (Map, empty, insert)
import Data.Map (singleton) as M
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import NewSyntax.Force (simulation)
import Unsafe.Coerce (unsafeCoerce)

-- we model the scope of the JavaScript "script" like this (see README for rationale)
data D3State model   = Context model (Map String NativeSelection)
type D3      model t = StateT (D3State model) Effect t

-- | get a reference to a simulation that we can use elsewhere (needs to go into context scope)
interpretSimulation :: forall model. Simulation -> 
                              (model -> Array Node) ->
                              (model -> Array Link) ->
                              D3 model Unit
interpretSimulation (Simulation simulation) getNodes getLinks =
  do
    (Context model scope) <- get
    let sim = initSimulationJS simulation.config
        nodes = getNodes model
        links = getLinks model
    updateScope sim (Just simulation.label)
    traverse_ interpretForce simulation.forces
    putNodesInSimulationJS sim nodes
    putLinksInSimulationJS sim links
    -- attach tick, end, drag handlers etc
    startSimulationJS
    pure unit

foreign import putNodesInSimulationJS :: Simulation -> Array Node -> Unit
foreign import putLinksInSimulationJS :: Simulation -> Array Link -> Unit
foreign import startSimulationJS      :: Simulation -> Unit
foreign import stopSimulationJS       :: Simulation -> Unit
foreign import forceManyJS            :: Simulation -> String -> Unit
foreign import forceCenterJS          :: Simulation -> String -> Number -> Number -> Unit
-- foreign import forceLinks             :: Simulation -> String -> Unit
foreign import forceCollideJS         :: Simulation -> String -> Number -> Unit
foreign import forceXJS               :: Simulation -> String -> Number -> Unit
foreign import forceYJS               :: Simulation -> String -> Number -> Unit
foreign import forceRadialJS          :: Simulation -> String -> Number -> Number -> Unit

interpretForce :: forall model. Simulation -> Force -> D3 model Unit
interpretForce simulation = 
  case _ of
    (Force label ForceMany)                 -> forceManyJS simulation label 
    (Force label (ForceCenter cx cy))       -> forceCenterJS simulation label cx cy
    -- (Force (ForceLink links idFn)) -> forceLinks
    (Force label (ForceCollide radius))     -> forceCollideJS simulation label radius
    (Force label (ForceX x))                -> forceXJS simulation label x
    (Force label (ForceY y))                -> forceYJS simulation label y
    (Force label (ForceRadial cx cy))       -> forceRadialJS simulation label cx cy
    (Force label Custom)                    -> pure unit -- do this later as needed

-- TODO fugly, fix later
interpretSelection :: forall model. Selection model -> D3 model Unit
interpretSelection = case _ of
  -- InitialSelection is the only one that we can start with (alt. use IxMonad)
  s@(InitialSelect r) -> go nullSelectionJS s 
  -- TODO raise error, or structure differently
  _ -> pure unit 

go :: forall model. NativeSelection -> Selection model -> D3 model Unit
go activeSelection selection = do 
  case selection of
    (InitialSelect r) -> do
          let root = spy "InitialSelect" $ d3SelectAllJS r.selector
          updateScope root (Just r.label)
          traverse_ (applyAttr root) r.attributes
          traverse_ (go root) r.children
          pure unit
          
    (Append r) -> do
          let appendSelection = spy "Append" d3AppendElementJS activeSelection (show r.element)
          updateScope appendSelection r.label
          traverse_ (applyAttr appendSelection) r.attributes
          traverse_ (go appendSelection) r.children
          pure unit

    (Join r) -> do
          (Context model scope) <- get
          let joinSelection = d3JoinJS activeSelection (show r.element) (r.projection model)
          updateScope joinSelection (Just r.label)
          enterSelection  <- go joinSelection r.enter
          -- updateSelection <- go joinSelection r.update
          -- exitSelection   <- go joinSelection r.exit
          -- need to get the enter, update and exit lined up and pass them all at once?
          -- if we get three selection handles back we can add to them later using names
          -- joinName.enter, joinName.update, joinName.exit for example
          pure unit

    (Transition _) -> pure unit
    NullSelection -> pure unit

updateScope :: forall model. NativeSelection -> Maybe String -> D3 model Unit
updateScope selection Nothing      = pure unit 
updateScope selection (Just label) =
  modify_ (\(Context model scope) -> Context model (insert label selection scope))

applyAttr :: forall model. NativeSelection -> Attr -> D3 model Unit
applyAttr selection = case _ of
    (StaticString attr value)      -> pure $ runSimpleAttrJS selection attr (stringToNativeJS value)
    (StaticNumber attr value)      -> pure $ runSimpleAttrJS selection attr (numberToNativeJS value)
    (StaticArrayNumber attr value) -> pure $ runSimpleAttrJS selection attr (arrayNumberToNativeJS value)

    (StringAttr attr fnD)          -> pure $ runDatumAttrJS selection attr fnD
    (NumberAttr attr fnD)          -> pure $ runDatumAttrJS selection attr fnD
    (ArrayNumberAttr attr fnD)     -> pure $ runDatumAttrJS selection attr fnD

    (StringAttrI attr fnDI)        -> pure $ runDatumIndexAttrJS selection attr fnDI
    (NumberAttrI attr fnDI)        -> pure $ runDatumIndexAttrJS selection attr fnDI
    (ArrayNumberAttrI attr fnDI)   -> pure $ runDatumIndexAttrJS selection attr fnDI


stringToNativeJS :: String -> NativeJS
stringToNativeJS = unsafeCoerce

numberToNativeJS :: Number -> NativeJS
numberToNativeJS = unsafeCoerce

arrayNumberToNativeJS :: Array Number -> NativeJS
arrayNumberToNativeJS = unsafeCoerce

-- all this FFI stuff can be made Effect Unit later 
foreign import runSimpleAttrJS :: NativeSelection -> String -> NativeJS -> Unit
foreign import runDatumAttrJS :: forall f. NativeSelection -> String -> f -> Unit
foreign import runDatumIndexAttrJS :: forall f. NativeSelection -> String -> f -> Unit
foreign import d3SelectAllJS :: String -> NativeSelection
foreign import d3AppendElementJS :: NativeSelection -> String -> NativeSelection
-- foreign import d3EnterElementJS :: NativeSelection -> String -> NativeSelection
foreign import d3JoinJS :: forall d. NativeSelection -> String -> d -> NativeSelection
foreign import d3JoinWithIndexJS :: forall d. NativeSelection -> d -> (d -> NativeJS) -> NativeSelection
foreign import nullSelectionJS :: NativeSelection
foreign import data NativeJS :: Type

foreign import initSimulationJS :: SimulationRecord -> Unit
