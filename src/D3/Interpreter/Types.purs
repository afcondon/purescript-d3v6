module D3.Interpreter.Types where

import Control.Monad.State (StateT)
import D3.Base (NativeSelection)
import Data.Map (Map, empty)
import Effect (Effect)

-- we model the scope of the JavaScript "script" like this (see README for rationale)
data D3State model   = Context model (Map String NativeSelection)

initialScope :: Map String NativeSelection
initialScope = empty

type D3 model t      = StateT(D3State model) Effect t
