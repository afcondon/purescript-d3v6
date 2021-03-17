module D3.Interpreter.Selection where

import Control.Monad.State (get, modify_)
import D3.Base (NativeSelection, Selection(..))
import D3.Interpreter.Attributes (applyAttr)
import D3.Interpreter.Foreign (d3AppendElementJS, d3EnterJS, d3SelectAllJS, nullSelectionJS)
import D3.Interpreter.Types (D3, D3State(..))
import Data.Array (reverse)
import Data.Foldable (traverse_)
import Data.Map (insert)
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Prelude (Unit, bind, discard, pure, show, unit, ($))
import Unsafe.Coerce (unsafeCoerce)

-- TODO fugly, fix later
runInitial :: forall model. Selection model -> D3 model NativeSelection
runInitial = case _ of
  -- selectInDOMion is the only one that we can start with (alt. use IxMonad)
  s@(InitialSelect r) -> go nullSelectionJS s 
  -- TODO handle extend selection cases needed for update / re-entrancy
  -- TODO raise error, or structure differently
  _ -> pure nullSelectionJS 

go :: forall model. NativeSelection -> Selection model -> D3 model NativeSelection
go activeSelection selection = do 
  case selection of
    (InitialSelect r) -> do
      let root = spy "selectInDOM" $ 
                 d3SelectAllJS r.selector
      traverse_ (go root) r.children
      pure root
          
    (Append r) -> do
      let appendSelection = spy "Append" $ 
                            d3AppendElementJS activeSelection (show r.element)
      updateScope appendSelection r.label
      traverse_ (applyAttr appendSelection) r.attributes
      traverse_ (go appendSelection) (reverse r.children) -- TODO why reverse children here?
      pure appendSelection

    (Join name element enter update exit (Just projection)) -> do
      (Context model scope) <- get
      let 
        joinSelection = spy "Join with projection: " $
                        d3EnterJS activeSelection (show element) (spy "submodel: " $ projection model)
      enterSelection  <- go joinSelection enter
      updateSelection <- go joinSelection update
      exitSelection   <- go joinSelection exit
      updateScope enterSelection (Just name)
      pure joinSelection
    (Join name element enter update exit Nothing) -> do
      (Context model scope) <- get
      let 
        joinSelection = spy "Join without projecting: " $
                        d3EnterJS activeSelection (show element) (spy "model" $ unsafeCoerce model) -- unsafeCoerce here is effectively "identity"
      enterSelection  <- go joinSelection enter
      updateSelection <- go joinSelection update -- if this is NullSelection, no call is made (see NullSelection interpreter below)
      exitSelection   <- go joinSelection exit   -- if this is NullSelection, no call is made (see NullSelection interpreter below)

      updateScope joinSelection (Just name)
      pure joinSelection

    -- TODO lookup the selection given by name and run interpreter on it with 
    (RunTimeSelection _ _) -> do
      pure activeSelection

    (Transition _) -> do
      pure activeSelection
    NullSelection  -> do
      pure activeSelection


-- TODO fugly, fix later
runUpdate :: forall model. Selection model -> D3 model NativeSelection
runUpdate = case _ of
  s@(InitialSelect r) -> go nullSelectionJS s 
  _ -> pure nullSelectionJS 

go' :: forall model. NativeSelection -> Selection model -> D3 model NativeSelection
go' activeSelection selection = do 
  case selection of
    (InitialSelect r) -> do
      let root = spy "selectInDOM" $ 
                 d3SelectAllJS r.selector
      updateScope root r.label
      traverse_ (applyAttr root) r.attributes
      traverse_ (go' root) r.children
      pure root
          
    (Append r) -> do
      let appendSelection = spy "Append" $ 
                            d3AppendElementJS activeSelection (show r.element)
      updateScope appendSelection r.label
      traverse_ (applyAttr appendSelection) r.attributes
      traverse_ (go' appendSelection) (reverse r.children) -- TODO why reverse children here?
      pure appendSelection

    (Join name element enter update exit (Just projection)) -> do
      (Context model scope) <- get
      let 
        joinSelection = spy "Join with projection: " $
                        d3EnterJS activeSelection (show element) (spy "submodel: " $ projection model)
      enterSelection  <- go' joinSelection enter
      updateScope enterSelection (Just name)
      pure joinSelection
    (Join name element enter update exit Nothing) -> do
      (Context model scope) <- get
      let 
        joinSelection = spy "Join without projecting: " $
                        d3EnterJS activeSelection (show element) (spy "model" $ unsafeCoerce model) -- unsafeCoerce here is effectively "identity"
      enterSelection  <- go' joinSelection enter
      updateScope enterSelection (Just name)
      -- updateSelection <- go joinSelection r.update
      -- exitSelection   <- go joinSelection r.exit
      -- need to get the enter, update and exit lined up and pass them all at once?
      -- if we get three selection handles back we can add to them later using names
      -- joinName.enter, joinName.update, joinName.exit for example
      pure joinSelection

    -- TODO lookup the selection given by name and run interpreter on it with 
    (RunTimeSelection _ _) -> do
      pure activeSelection

    (Transition _) -> do
      pure activeSelection
    NullSelection  -> do
      pure activeSelection

updateScope :: forall model. NativeSelection -> Maybe String -> D3 model Unit
updateScope selection Nothing      = pure unit 
updateScope selection (Just label) =
  modify_ (\(Context model scope) -> Context model (insert label selection scope))

