module Main where

import Prelude

import Affjax (Error, get)
import Affjax.ResponseFormat as ResponseFormat
import D3.Example.Force (chart) as Force
import D3.Example.Minimal (chart) as Minimal
import D3.Example.Tree (chart) as Tree
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

renderUsing :: forall r. (String -> Unit) -> Either Error { body ∷ String | r } -> Unit
renderUsing f (Right { body } ) = f body
renderUsing _ (Left err)        = unit

getWindowWidthHeight :: Effect (Tuple Int Int)
getWindowWidthHeight = do
  win <- window
  width <- innerWidth win
  height <- innerHeight win
  pure $ Tuple width height


main :: Effect Unit
main = launchAff_ do -- Aff 
  widthHeight <- liftEffect getWindowWidthHeight
  forceJSON <- get ResponseFormat.string "http://localhost:1234/miserables.json"
  let forceData = renderUsing (Force.chart widthHeight) forceJSON
  -- treeJSON <- get ResponseFormat.string "http://localhost:1234/miserables.json"
  -- let treeData = renderUsing Tree.chart treeJSON
  let _ = Minimal.chart 2
  liftEffect $ log "🍝"

