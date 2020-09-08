module Main where

import Prelude

import Affjax (Error, get)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (runStateT)
import D3.Interpreter (D3State(..), interpretSelection)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Map (empty)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import NewSyntax.Force (Model, chart, readJSONJS)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

readModelFromFileContents :: forall r. Either Error { body ∷ String | r } -> Model
readModelFromFileContents (Right { body } ) = readJSONJS body
readModelFromFileContents (Left err)        = { links: [], nodes: [] }

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
  let model = readModelFromFileContents forceJSON
  result <- liftEffect $ runStateT (interpretSelection forceChart) (Context model empty)
  liftEffect $ log "🍝"

