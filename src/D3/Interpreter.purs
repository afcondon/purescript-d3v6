module D3.Interpreter where

import Prelude

import Control.Monad.State (StateT, get, modify_, put)
import D3.Base (Attr(..), DragBehavior(..), Force(..), ForceType(..), NativeSelection, Selection(..), Simulation(..), SimulationConfig, TickMap)
import Data.Array (concatMap, foldl, fromFoldable, reverse, (:))
import Data.Bifunctor (lmap)
import Data.Foldable (traverse_)
import Data.Map (Map, empty, insert, lookup, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
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
foreign import runDatumTextJS      :: forall f. NativeSelection           -> f -> Unit
foreign import runDatumIndexAttrJS :: forall f. NativeSelection -> String -> f -> Unit
foreign import d3SelectAllJS       ::           String -> NativeSelection
foreign import d3AppendElementJS   ::           NativeSelection -> String -> NativeSelection
-- foreign import d3EnterElementJS :: NativeSelection -> String -> NativeSelection
foreign import d3JoinJS            :: forall d. NativeSelection -> String -> d -> NativeSelection
foreign import d3JoinWithIndexJS   :: forall d. NativeSelection -> d -> (d -> NativeJS) -> NativeSelection
foreign import nullSelectionJS     ::           NativeSelection

-- | foreign types associated with Force Layout Simulation
foreign import initSimulationJS :: SimulationConfig -> NativeSelection
-- TODO tick functions should be named too, so this should be param'd with a tick NativeSelection too!
foreign import addAttrFnToTickJS :: NativeSelection -> Attr -> Unit
foreign import attachTickFnToSimulationJS :: NativeSelection -> Unit
foreign import attachDefaultDragBehaviorJS :: NativeSelection -> NativeSelection -> Unit
foreign import putNodesInSimulationJS :: NativeSelection -> Array NativeJS -> Array NativeJS
foreign import putLinksInSimulationJS :: NativeSelection -> Array NativeJS -> Array NativeJS
foreign import startSimulationJS      :: NativeSelection -> Unit
foreign import stopSimulationJS       :: NativeSelection -> Unit
foreign import setAlphaTargetJS       :: NativeSelection -> Number -> Unit
foreign import forceManyJS            :: NativeSelection -> String -> Unit
foreign import forceCenterJS          :: NativeSelection -> String -> Number -> Number -> Unit
foreign import forceCollideJS         :: NativeSelection -> String -> Number -> Unit
foreign import forceXJS               :: NativeSelection -> String -> Number -> Unit
foreign import forceYJS               :: NativeSelection -> String -> Number -> Unit
foreign import forceRadialJS          :: NativeSelection -> String -> Number -> Number -> Unit

-- we model the scope of the JavaScript "script" like this (see README for rationale)
data D3State model   = Context model (Map String NativeSelection)

initialScope :: Map String NativeSelection
initialScope = empty

type D3 model t      = StateT (D3State model) Effect t

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
    (Force label (ForceCollide radius))     -> pure $ forceCollideJS simulation label radius
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
          traverse_ (go appendSelection) (reverse r.children)
          pure unit

    (Join r) -> do
          (Context model scope) <- get
          let joinSelection = d3JoinJS activeSelection (show r.element) (r.projection model)
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

-- interprets and calls D3 directly, as opposed to storing attr associations on JS side,
-- as is needed for, for example tick function
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

    (TextAttr fnD)                 -> pure $ runDatumTextJS selection fnD

stringToNativeJS :: String -> NativeJS
stringToNativeJS = unsafeCoerce

numberToNativeJS :: Number -> NativeJS
numberToNativeJS = unsafeCoerce

arrayNumberToNativeJS :: Array Number -> NativeJS
arrayNumberToNativeJS = unsafeCoerce
