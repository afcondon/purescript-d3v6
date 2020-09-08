module D3.Interpreter where

import Prelude

import Control.Monad.State (StateT(..), get, modify_, put)
import D3.Base (Attr(..), NativeSelection, Selection(..), Simulation)
import Data.Foldable (foldl, traverse_)
import Data.Map (Map, empty, insert)
import Data.Map (singleton) as M
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

-- | get a reference to a simulation that we can use elsewhere (needs to go into context scope)
interpretSimulation :: forall model. Simulation -> D3 model Unit
interpretSimulation simulation = pure unit

-- we're going to model the scope of the JavaScript "script" like this
-- this will allow us to have relatively low-overhead continuity across 
-- parts of the script
data D3State model   = Context model (Map String NativeSelection)
type D3      model t = StateT (D3State model) Effect t

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
          let appendSelection = spy "Append" $ d3AppendElementJS activeSelection (show r.element)
          updateScope appendSelection r.label
          traverse_ (applyAttr appendSelection) r.attributes
          traverse_ (go appendSelection) r.children
          pure unit

    (Join r) -> do
          (Context model scope) <- get
          let joinSelection = d3JoinJS activeSelection (r.projection model)
          updateScope joinSelection (Just r.label)
          enterSelection  <- go joinSelection r.enter
          updateSelection <- go joinSelection r.update
          exitSelection   <- go joinSelection r.exit
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

foreign import runSimpleAttrJS :: NativeSelection -> String -> NativeJS -> Unit
foreign import runDatumAttrJS :: forall f. NativeSelection -> String -> f -> Unit
foreign import runDatumIndexAttrJS :: forall f. NativeSelection -> String -> f -> Unit
foreign import d3SelectAllJS :: String -> NativeSelection
foreign import d3AppendElementJS :: NativeSelection -> String -> NativeSelection
foreign import d3JoinJS :: forall d. NativeSelection -> d -> NativeSelection
foreign import d3JoinWithIndexJS :: forall d. NativeSelection -> d -> (d -> NativeJS) -> NativeSelection
foreign import nullSelectionJS :: NativeSelection
foreign import data NativeJS :: Type

-- newInterpreter :: forall model. D3Monad Unit
-- newInterpreter = runStateT 



-- -- | Run a computation in the `StateT` monad, discarding the final state.
-- evalStateT :: forall s m a. Functor m => StateT s m a -> s -> m a
-- evalStateT (StateT m) s = fst <$> m s

-- -- | Run a computation in the `StateT` monad discarding the result.
-- execStateT :: forall s m a. Functor m => StateT s m a -> s -> m s
-- execStateT (StateT m) s = snd <$> m s

