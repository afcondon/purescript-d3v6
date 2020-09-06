module D3.Interpreter where

import Prelude

import Control.Monad.State (class MonadState, StateT(..), get, put, runStateT)
import D3.Base (Attr(..), D3Selection, Selection(..), Simulation)
import Data.Foldable (foldl)
import Data.Map (Map, empty, insert)
import Data.Map (singleton) as M
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

-- | get a reference to a simulation that we can use elsewhere (needs to go into context scope)
interpretSimulation :: forall model. Simulation -> D3Monad model Unit
interpretSimulation simulation = pure unit

-- we're going to model the scope of the JavaScript "script" like this
-- this will allow us to have relatively low-overhead continuity across 
-- parts of the script
data D3Context model = Context model (Map String D3Selection)

derive instance functorD3Context :: Functor D3Context

newtype D3Monad model a = D3Monad (StateT (D3Context model) Effect a)

-- look at all the boilerplate we can save here!
derive newtype instance functorD3Monad     :: Functor     (D3Monad model)
derive newtype instance applyD3Monad       :: Apply       (D3Monad model)
derive newtype instance applicativeD3Monad :: Applicative (D3Monad model)
derive newtype instance bindD3Monad        :: Bind        (D3Monad model)
derive newtype instance monadD3Monad       :: Monad       (D3Monad model)

derive newtype instance monadStateD3Monad  :: MonadState (D3Context model) (D3Monad model)


-- | Run a computation in the `StateT` monad.
-- runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)
-- runStateT (StateT s) = s
interpretSelection :: forall model. model -> Selection model -> D3Monad model Unit
interpretSelection = case _ of
  s@(InitialSelect r) -> go nullSelectionJS s -- TODO fugly, fix later
  _ -> pure unit -- TODO raise error, InitialSelection is the only one that we can start with (alt. use IxMonad)

go :: forall model. D3Selection -> Selection model -> D3Monad model Unit
go activeSelection = case _ of
  (InitialSelect r) -> do
        let root = d3SelectAllJS r.selector
        -- put $ initialScope r.model root r.label
        (applyAttr root) <$> r.attributes
        (go root)<$> r.children
        pure unit
        
  (Append r) -> do
        let selection = d3AppendElementJS activeSelection (show r.element)
        -- updateScope selection r.label
        (applyAttr selection) <$> r.attributes
        (go selection) <$> r.children
        pure unit

  (Join r) -> do
        (Context model scope) <- get 
        let selection = d3JoinJS (r.projection model)
        updateScope selection (Just r.label)
        -- need to get the enter, update and exit lined up and pass them all at once?
        -- if we get three selection handles back we can add to them later using names
        -- joinName.enter, joinName.update, joinName.exit for example
        pure unit

  (Transition _) -> unit
  NullSelection -> unit

initialScope :: forall model. model -> D3Selection -> String -> D3Context model
initialScope model selection label = Context model (M.singleton label selection)

updateScope :: forall model. D3Selection -> Maybe String -> D3Monad model Unit
updateScope selection label =
  case label of
    (Just name) -> do
      (Context model scope) <- get
      put $ Context model (insert name selection scope)
      pure unit
    Nothing -> pure unit 

applyAttr :: forall model. D3Selection -> Attr -> D3Monad model Unit
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

foreign import runSimpleAttrJS :: D3Selection -> String -> NativeJS -> Unit
foreign import runDatumAttrJS :: forall f. D3Selection -> String -> f -> Unit
foreign import runDatumIndexAttrJS :: forall f. D3Selection -> String -> f -> Unit
foreign import d3SelectAllJS :: String -> D3Selection
foreign import d3AppendElementJS :: D3Selection -> String -> D3Selection
foreign import d3JoinJS :: forall d. d -> D3Selection
foreign import nullSelectionJS :: D3Selection
foreign import data NativeJS :: Type

-- newInterpreter :: forall model. D3Monad Unit
-- newInterpreter = runStateT 



-- -- | Run a computation in the `StateT` monad, discarding the final state.
-- evalStateT :: forall s m a. Functor m => StateT s m a -> s -> m a
-- evalStateT (StateT m) s = fst <$> m s

-- -- | Run a computation in the `StateT` monad discarding the result.
-- execStateT :: forall s m a. Functor m => StateT s m a -> s -> m s
-- execStateT (StateT m) s = snd <$> m s

