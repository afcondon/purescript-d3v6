module Main where

import Prelude

import Affjax (Error, get)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (runStateT)
import D3.Base (Selection, emptySelection)
import D3.Interpreter (interpretSelection, interpretSimulation, D3State(..))
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Map (empty)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import NewSyntax.Force (Model, chart, simulation, readJSONJS)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

readModelFromFileContents :: forall r. (Tuple Number Number) -> Either Error { body ∷ String | r } -> Model
readModelFromFileContents widthHeight (Right { body } ) = readJSONJS body
readModelFromFileContents _ (Left err)                  = { links: [], nodes: [] }

getWindowWidthHeight :: Effect (Tuple Number Number)
getWindowWidthHeight = do
  win <- window
  width <- innerWidth win
  height <- innerHeight win
  pure $ Tuple (toNumber width) (toNumber height)

main :: Effect Unit
main = launchAff_ do -- Aff 
  widthHeight <- liftEffect getWindowWidthHeight
  forceJSON <- get ResponseFormat.string "http://localhost:1234/miserables.json"
  let forceChart = chart widthHeight
  let model = { links: [], nodes: [] } :: Model
  -- liftEffect $ interpretSimulation simulation
  result <- liftEffect $ runStateT (interpretSelection forceChart) (Context model empty)
  liftEffect $ log "🍝"

