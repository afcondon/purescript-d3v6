module Main where

import Prelude

import Affjax (get)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (StateT, runStateT)
import D3.Base (Selection)
import D3.Interpreter (D3State(..), initialScope, interpretSelection, interpretSimulation)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import NewSyntax.Force (Model, makeModel, chart, readModelFromFileContents, simulation, getLinks, getNodes)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

getWindowWidthHeight :: Effect (Tuple Number Number)
getWindowWidthHeight = do
  win <- window
  width <- innerWidth win
  height <- innerHeight win
  pure $ Tuple (toNumber width) (toNumber height)

-- interpreter :: forall model. Selection model -> StateT (D3State model) Effect Unit
interpreter :: Selection Model -> StateT (D3State Model) Effect Unit
interpreter forceChart = do
  interpretSimulation simulation getNodes getLinks makeModel
  interpretSelection forceChart

main :: Effect Unit
main = launchAff_ do -- Aff 
  widthHeight    <- liftEffect getWindowWidthHeight
  forceJSON      <- get ResponseFormat.string "http://localhost:1234/miserables.json"
  let forceChart = chart widthHeight
  let fileData   = readModelFromFileContents forceJSON
  let model      = makeModel fileData.links fileData.nodes
  liftEffect $ runStateT (interpreter forceChart) (Context model initialScope)

