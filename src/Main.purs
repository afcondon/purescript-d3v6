module Main where

import Prelude

import Affjax (Error, get)
import Affjax.ResponseFormat as ResponseFormat
import D3.Base (Selection, emptySelection)
import D3.Interpreter (interpretSelection, interpretSimulation)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import NewSyntax.Force (Model, chart, simulation)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

createSelection :: forall r. (String -> Selection Model) -> Either Error { body ∷ String | r } -> Selection Model
createSelection f (Right { body } ) = f body
createSelection _ (Left err)        = emptySelection

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
  let forceChart = createSelection  (chart widthHeight) forceJSON
  liftEffect $ interpretSimulation simulation
  liftEffect $ interpretSelection forceChart
  liftEffect $ log "🍝"

