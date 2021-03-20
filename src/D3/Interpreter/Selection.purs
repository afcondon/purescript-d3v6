module D3.Interpreter.Selection where

import Prelude

import D3.Base (Element(..), NativeSelection, Selection(..))
import D3.Interpreter.Attributes (applyAttr)
import D3.Interpreter.Foreign (d3AppendElementJS, d3JoinJS, d3SelectAllJS, nullSelectionJS)
import Data.Array (reverse)
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)

interpretSelection :: forall model. model -> NativeSelection -> Selection model -> NativeSelection
interpretSelection model activeSelection = do 
  case _ of
    (InitialSelect r) -> do
      let root = spy "selectInDOM" $ 
                 d3SelectAllJS r.selector
          _ = (interpretSelection model root) <$> r.children
      root
          
    (Append r) -> do
      let appendSelection = spy "Append" $ 
                            d3AppendElementJS activeSelection (show r.element)
          _ = (applyAttr appendSelection) <$> r.attributes
          _ = (interpretSelection model appendSelection) <$> (reverse r.children) -- TODO why reverse children here?
      appendSelection

    (Join { element, projection, selections }) -> do
      let
        -- in these three callbacks the Selection model is pre-loaded on PureScript side
        -- and the callback from the join function provides the enter/update/exit selection to act upon
        -- the callbacks must return the same selection they were given for things to work correctly
        
        enterCallback :: Selection model
            -> (NativeSelection -> NativeSelection) -- JS callback
        enterCallback selection enterSelection = do
          let _ = log "callback from join enter"
          interpretSelection model enterSelection selections.enter

        updateCallback :: Maybe (Selection model)
            -> (NativeSelection -> NativeSelection)
        updateCallback maybeUpdate updateSelection = do
          let _ = log "callback from join update"
          case maybeUpdate of
            (Just update) -> interpretSelection model updateSelection update
            Nothing       -> updateSelection -- update => update, in D3 terms

        exitCallback :: Maybe (Selection model)
            -> (NativeSelection -> NativeSelection)
        exitCallback maybeExit exitSelection = do
          let _ = log "callback from join exit"
          case maybeExit of
            (Just exit) -> interpretSelection model exitSelection exit
            Nothing     -> exitSelection -- exit => exit, in D3 terms

        dataForJoin = 
          case projection of
            Nothing   -> unsafeCoerce model
            (Just fn) -> fn model

        joinSelection = spy "Join: " $
                        d3JoinJS activeSelection (show element) (spy "submodel: " dataForJoin)
                          { enter:  enterCallback selections.enter
                          , update: updateCallback selections.update
                          , exit:   exitCallback selections.exit }

      joinSelection


    -- TODO lookup the selection given by name and interpret interpretSelectioner on it with 
    (RunTimeSelection _ _) -> do
      activeSelection

    -- (Transition _) -> do
    --   activeSelection
    (Update _)  -> do
      activeSelection
    (Exit _)  -> do
      activeSelection

    NullSelection  -> do
      activeSelection

