module D3.Interpreter.Selection where

import Control.Monad.State (get, modify_)
import D3.Base (NativeSelection, Selection(..))
import D3.Interpreter.Attributes (applyAttr)
import D3.Interpreter.Foreign (d3AppendElementJS, d3JoinJS, d3SelectAllJS, nullSelectionJS)
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
  s@(InitialSelect r) -> run nullSelectionJS s 
  -- TODO handle extend selection cases needed for update / re-entrancy
  -- TODO raise error, or structure differently
  _ -> pure nullSelectionJS 

run :: forall model. NativeSelection -> Selection model -> D3 model NativeSelection
run activeSelection selection = do 
  case selection of
    (InitialSelect r) -> do
      let root = spy "selectInDOM" $ 
                 d3SelectAllJS r.selector
      traverse_ (run root) r.children
      pure root
          
    (Append r) -> do
      let appendSelection = spy "Append" $ 
                            d3AppendElementJS activeSelection (show r.element)
      traverse_ (applyAttr appendSelection) r.attributes
      traverse_ (run appendSelection) (reverse r.children) -- TODO why reverse children here?
      pure appendSelection

    (Join { element, projection, selections}) -> do
      (Context model scope) <- get
      let 
        dataForJoin = 
          case projection of
            Nothing   -> unsafeCoerce model
            (Just fn) -> fn model
        joinSelection = spy "Join1 with projection: " $
                        d3JoinJS activeSelection (show element) (spy "submodel: " dataForJoin)
                          { enter: (\ns -> ns), update: (\ns -> ns), exit: (\ns -> ns) }
      pure joinSelection


    -- TODO lookup the selection given by name and run interpreter on it with 
    (RunTimeSelection _ _) -> do
      pure activeSelection

    -- (Transition _) -> do
    --   pure activeSelection
    (Update _)  -> do
      pure activeSelection
    (Exit _)  -> do
      pure activeSelection

    NullSelection  -> do
      pure activeSelection


updateScope :: forall model. NativeSelection -> Maybe String -> D3 model Unit
updateScope selection Nothing      = pure unit 
updateScope selection (Just label) =
  modify_ (\(Context model scope) -> Context model (insert label selection scope))

