module D3.Interpreter.Types where

import Control.Monad.State (StateT)
import D3.Base (NativeSelection)
import Data.Map (Map, empty)
import Effect (Effect)

-- we model the scope of the JavaScript "script" like this (see README for rationale)
data D3State model   = Context model (Map String NativeSelection)

initialState :: forall model. model -> D3State model
initialState model = Context model empty

-- TODO alternatively, modify state with new model
updateState :: forall model. model -> D3State model -> D3State model
updateState model (Context _ scope) = Context model scope

type D3 model t      = StateT(D3State model) Effect t
