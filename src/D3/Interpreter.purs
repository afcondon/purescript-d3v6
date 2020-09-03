module D3.Interpreter where

import D3.Base (Selection)
import Prelude (Unit, pure, unit)

import Effect (Effect)

interpret :: forall model. model -> Selection model -> Effect Unit
interpret model chart = pure unit