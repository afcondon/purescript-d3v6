module D3.Interpreter where

import Prelude

import Control.Monad.State (StateT(..), get, put)
import D3.Base (Attr(..), NativeSelection, Selection(..), Simulation)
import Data.Foldable (foldl)
import Data.Map (Map, empty, insert)
import Data.Map (singleton) as M
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

-- | get a reference to a simulation that we can use elsewhere (needs to go into context scope)
interpretSimulation :: forall model. Simulation -> D3 model Unit
interpretSimulation simulation = pure unit

-- we're going to model the scope of the JavaScript "script" like this
-- this will allow us to have relatively low-overhead continuity across 
-- parts of the script
data D3State model = Context model (Map String NativeSelection)
type D3 model t = StateT (D3State model) Effect t


interpretSelection :: forall model. NativeSelection -> Selection model -> D3 model Unit
interpretSelection d3select (InitialSelect r) = do
    let root = d3SelectAllJS r.selector
    put $ Context r.model (M.singleton r.label d3select) :: D3 model Unit
    let _ = (applyAttr root) <$> r.attributes
        _ = (interpretSelection root) <$> r.children
    pure unit

interpretSelection _ _ = pure unit


-- -- TODO fugly, fix later
-- -- TODO raise error, InitialSelection is the only one that we can start with (alt. use IxMonad)
interpretSelection' :: forall model. Selection model -> D3 model Unit
interpretSelection' = case _ of
  s@(InitialSelect r) -> go nullSelectionJS s 
  _ -> pure unit 

go :: forall model. NativeSelection -> Selection model -> D3 model Unit
go activeSelection selection = do 
  case selection of
    (InitialSelect r) -> do
          let root = d3SelectAllJS r.selector
          put $ Context r.model (M.singleton r.label root) :: D3 model Unit
          let _ = (applyAttr root) <$> r.attributes
              _ = (go root)<$> r.children
          pure unit
          
    (Append r) -> do
          let selection = d3AppendElementJS activeSelection (show r.element)
          updateScope selection r.label
          let _ = (applyAttr selection) <$> r.attributes
              _ = (go selection) <$> r.children
          pure unit

    (Join r) -> do
          (Context model scope) <- get
          let selection = d3JoinJS activeSelection (r.projection model)
              _ = updateScope selection (Just r.label)
          -- need to get the enter, update and exit lined up and pass them all at once?
          -- if we get three selection handles back we can add to them later using names
          -- joinName.enter, joinName.update, joinName.exit for example
          pure unit

    (Transition _) -> pure unit
    NullSelection -> pure unit

initialScope :: forall model. model -> NativeSelection -> String -> D3State model
initialScope model selection label = Context model (M.singleton label selection)

updateScope :: forall model. NativeSelection -> Maybe String -> D3 model Unit
updateScope selection label =
  case label of
    (Just name) -> do
      context <- get
      let (Context model scope) = context
      put $ Context model (insert name selection scope)
      pure unit
    Nothing -> pure unit 

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

