module Main where

import Prelude

import Affjax (get) as AJAX
import Affjax (printError)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (StateT, runStateT)
import D3.Base (Selection)
import D3.Interpreter (D3State(..), initialScope, interpretDrag, interpretSelection, interpretSimulation, interpretTickMap, startSimulation)
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import NewSyntax.Force as Force
import NewSyntax.GUP as GUP
import NewSyntax.Tree as Tree
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

getWindowWidthHeight :: Effect (Tuple Number Number)
getWindowWidthHeight = do
  win <- window
  width <- innerWidth win
  height <- innerHeight win
  pure $ Tuple (toNumber width) (toNumber height)

forceInterpreter :: Selection Force.Model -> StateT (D3State Force.Model) Effect Unit
forceInterpreter forceChart = do
  simulation <- interpretSimulation Force.simulation Force.getNodes Force.getLinks Force.makeModel
  interpretSelection forceChart
  interpretTickMap simulation Force.myTickMap
  interpretDrag Force.myDrag
  startSimulation simulation


main :: Effect Unit
main = launchAff_ do -- Aff 
  widthHeight    <- liftEffect getWindowWidthHeight

  let numberOfExamples = 3.0
      widthHeight' = rmap (\h -> h/numberOfExamples) widthHeight

  -- first, a force layout example
  forceJSON      <- AJAX.get ResponseFormat.string "http://localhost:1234/miserables.json"
  let forceChart = Force.chart widthHeight'
  let fileData   = Force.readModelFromFileContents forceJSON
  let forceModel = Force.makeModel fileData.links fileData.nodes
  let forceScope = initialScope
  _ <- liftEffect $ runStateT (forceInterpreter forceChart) (Context forceModel forceScope)

  -- then a radial tree
  treeJSON      <- AJAX.get ResponseFormat.string "http://localhost:1234/flare-2.json"
  let treeChart = Tree.chart widthHeight'
  let treeScope = initialScope
  case Tree.readModelFromFileContents widthHeight' treeJSON of
    (Left error) -> liftEffect $ log $ printError error
    (Right treeModel) -> liftEffect $ 
                         runStateT (interpretSelection treeChart) (Context treeModel treeScope) *> pure unit

  -- then the General Update Pattern example
  let lettersChart = GUP.chartInit widthHeight'
  let letters = GUP.makeModel $ toCharArray "abcdefghijklmnopqrstuvwxyz"
  let lettersScope = initialScope
  liftEffect $ runStateT (interpretSelection lettersChart) (Context letters lettersScope)
  -- TODO now we need to use the monadic context inside StateT to (repeatedly) add the GUP.chartUpdate 