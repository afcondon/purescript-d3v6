module Main where

import Prelude

import Affjax (get) as AJAX
import Affjax (printError)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (StateT, runStateT)
import D3.Base (NativeSelection, Selection)
import D3.Interpreter (D3State(..), initialState, interpretDrag, interpretSelection, interpretSimulation, interpretTickMap, startSimulation) as I
import D3.Interpreter (interpretSelection)
import D3.Interpreter.Foreign (nullSelectionJS)
import D3.Interpreter.Types (updateState)
import D3.InterpreterM (D3State(..), initialState, interpretDrag, runInitial, interpretSimulation, interpretTickMap, startSimulation) as IM
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
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

forceInterpreterM :: Selection Force.Model -> StateT (IM.D3State Force.Model) Effect Unit
forceInterpreterM forceChart = do
  simulation <- IM.interpretSimulation Force.simulation Force.getNodes Force.getLinks Force.makeModel
  _ <- IM.runInitial forceChart
  IM.interpretTickMap simulation Force.myTickMap
  IM.interpretDrag Force.myDrag
  IM.startSimulation simulation
  pure unit -- should be we returning the updated Selection? and the Simulation? 

-- TODO take the file reading stuff out so that we can see the essentials
main :: Effect Unit
main = launchAff_ do -- Aff 
  widthHeight    <- liftEffect getWindowWidthHeight

  let numberOfExamples = 3.0
      widthHeight' = rmap (\h -> h/numberOfExamples) widthHeight

  -- first, a force layout example
  log "Force layout example"
  forceJSON      <- AJAX.get ResponseFormat.string "http://localhost:1234/miserables.json"
  let fileData     = Force.readModelFromFileContents forceJSON
  let forceChart   = Force.chart widthHeight'
  let forceModel   = Force.makeModel fileData.links fileData.nodes

  _ <- liftEffect $ 
       runStateT (forceInterpreterM forceChart) (IM.initialState forceModel)

  -- then a radial tree
  log "Radial tree example"
  treeJSON      <- AJAX.get ResponseFormat.string "http://localhost:1234/flare-2.json"
  let treeChart    = Tree.chart widthHeight'
  case Tree.readModelFromFileContents widthHeight' treeJSON of
    (Left error) -> liftEffect $ log $ printError error

    (Right treeModel) -> liftEffect $ 
                         runStateT (IM.runInitial treeChart) (IM.initialState treeModel) *> pure unit

  -- then the General Update Pattern example
  log "General Update Pattern example"
  let 
    lettersChart = GUP.chartInit widthHeight'
    letters1     = toCharArray "abcdefghijklmnopqrstuvwxyz"
    letters2     = toCharArray "acefghiklnpqrtuwyz"

  let _ = I.interpretSelection letters1 nullSelectionJS lettersChart

  _ <- delay $ Milliseconds 2000.0

  let _ = I.interpretSelection letters2 nullSelectionJS lettersChart
  -- _           <- liftEffect $
  --                runStateT (runInitial lettersChart) (updateState letters2 s)
  -- TODO now we need to use the monadic context inside StateT to (repeatedly) add the GUP.chartUpdate
  -- and we really want the NativeSelection to be passed in via scope, right?
  -- _ <- liftEffect $ runStateT (interpretSelection GUP.chartUpdate ) (Context letters2 lettersScope)
  -- _ <- liftEffect $ runStateT (interpretSelection chartUpdate ) (Context letters2 lettersScope)
  -- _ <- liftEffect $ runStateT (interpretSelection chartUpdate ) (Context letters2 lettersScope)
  pure unit