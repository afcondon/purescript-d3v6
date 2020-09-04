module D3.Interpreter where

import D3.Base (Selection, Simulation(..))
import Effect (Effect)
import Prelude (Unit, pure, unit)

interpretSelection :: forall model. Selection model -> Effect Unit
interpretSelection chart = pure unit

interpretSimulation :: Simulation -> Effect Unit
interpretSimulation simulation = pure unit