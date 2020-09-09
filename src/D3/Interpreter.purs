module D3.Interpreter where

import Prelude

import Control.Monad.State (StateT, get, modify_)
import D3.Base (Attr(..), Force(..), ForceType(..), NativeSelection, Selection(..), Simulation(..), SimulationConfig)
import Data.Foldable (traverse_)
import Data.Map (Map, insert)
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

-- | right upfront we're just going to hand wave over the types given to D3
-- | design decision here to prefer (aliased) `unsafeCoerce` to complex wrappers
-- | D3 WILL modify any data you give it, it will also require types that CANNOT be
-- | expressed in PureScript AND also transparently shared with JavaScript 
-- | (ie Variants, Exists, Nullable etc)
-- | IMPORTANT - coercions should not be visible outside this module
foreign import data NativeJS       ::           Type

-- | foreign types associated with D3 Selection 
-- all this FFI stuff can be made Effect Unit later 
foreign import runSimpleAttrJS     ::           NativeSelection -> String -> NativeJS -> Unit
foreign import runDatumAttrJS      :: forall f. NativeSelection -> String -> f -> Unit
foreign import runDatumIndexAttrJS :: forall f. NativeSelection -> String -> f -> Unit
foreign import d3SelectAllJS       ::           String -> NativeSelection
foreign import d3AppendElementJS   ::           NativeSelection -> String -> NativeSelection
-- foreign import d3EnterElementJS :: NativeSelection -> String -> NativeSelection
foreign import d3JoinJS            :: forall d. NativeSelection -> String -> d -> NativeSelection
foreign import d3JoinWithIndexJS   :: forall d. NativeSelection -> d -> (d -> NativeJS) -> NativeSelection
foreign import nullSelectionJS     ::           NativeSelection

-- | foreign types associated with Force Layout Simulation
foreign import initSimulationJS :: SimulationConfig -> NativeSelection
-- | TODO NB NativeSelection here includes "NativeSimulation" and so is mis-named

foreign import putNodesInSimulationJS :: NativeSelection -> Array NativeJS -> Unit
foreign import putLinksInSimulationJS :: NativeSelection -> Array NativeJS -> Unit
foreign import startSimulationJS      :: NativeSelection -> Unit
foreign import stopSimulationJS       :: NativeSelection -> Unit
foreign import forceManyJS            :: NativeSelection -> String -> Unit
foreign import forceCenterJS          :: NativeSelection -> String -> Number -> Number -> Unit
-- foreign import forceLinks             :: NativeSelection -> String -> Unit
foreign import forceCollideJS         :: NativeSelection -> String -> Number -> Unit
foreign import forceXJS               :: NativeSelection -> String -> Number -> Unit
foreign import forceYJS               :: NativeSelection -> String -> Number -> Unit
foreign import forceRadialJS          :: NativeSelection -> String -> Number -> Number -> Unit

-- we model the scope of the JavaScript "script" like this (see README for rationale)
data D3State model   = Context model (Map String NativeSelection)
type D3      model t = StateT (D3State model) Effect t

-- |                   FORCE LAYOUT (SIMULATION) interpreter

-- | get a reference to a simulation that we can then use elsewhere
-- | ie having interpreted a Selection such that the DOM is set up to run a simulation
interpretSimulation :: forall model node link. Simulation -> 
                              (model -> Array node) ->
                              (model -> Array link) ->
                              D3 model Unit
interpretSimulation (Simulation r) getNodes getLinks =
  do
    (Context model scope) <- get
    let sim = initSimulationJS r.config
        nodes = nativeNodes $ getNodes model
        links = nativeLinks $ getLinks model
    updateScope sim (Just r.label)
    traverse_ (interpretForce sim) r.forces
    let _ = putNodesInSimulationJS sim nodes
        _ = putLinksInSimulationJS sim links
    -- attach tick, end, drag handlers etc
        _ = startSimulationJS
    pure unit
  where
    nativeNodes = unsafeCoerce :: Array node -> Array NativeJS
    nativeLinks = unsafeCoerce :: Array link -> Array NativeJS



interpretForce :: forall model. NativeSelection -> Force -> D3 model Unit
interpretForce simulation = do
  case _ of
    (Force label ForceMany)                 -> pure $ forceManyJS simulation label 
    (Force label (ForceCenter cx cy))       -> pure $ forceCenterJS simulation label cx cy
    -- (Force (ForceLink links idFn)) -> pure $ forceLinks
    (Force label (ForceCollide radius))     -> pure $ forceCollideJS simulation label radius
    (Force label (ForceX x))                -> pure $ forceXJS simulation label x
    (Force label (ForceY y))                -> pure $ forceYJS simulation label y
    (Force label (ForceRadial cx cy))       -> pure $ forceRadialJS simulation label cx cy
    (Force label Custom)                    -> pure $ unit -- do this later as needed


-- |               SELECTION interpreter

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
