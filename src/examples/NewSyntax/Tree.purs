module NewSyntax.Tree (
    Model, makeModel, Tree(..) -- no constructor
) where

import D3.Base

import Affjax (Error)
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Map (fromFoldable)
import Data.Tuple (Tuple(..))
import Math (pi, sqrt)
import Unsafe.Coerce (unsafeCoerce)
import Prelude (const, identity, negate, show, (*), (/), (-), (>=), (==), (<), unit, (<>), ($), (/))

data Tree a = Node a (Array (Tree a))
-- this probably belongs in d3 base?
type TreeConfig a = {
    width      :: Int
  , height     :: Int
  , separation :: Tree a -> Tree a -> Int
}

-- this is the model that this particular "chart" / simulation uses
data Model a = Model {
      tree :: Tree a
    , d3Tree :: D3Tree
    , config :: TreeConfig a
}

makeModel :: forall a. TreeConfig a -> Tree a -> Model a
makeModel config tree = Model {
    tree
  , d3Tree: d3Hierarchy tree
  , config
}

-- this is an opaque type behind which hides the data type of the Purescript tree that was converted
foreign import data ModelData :: Type
foreign import data RecursiveD3TreeNode :: Type
-- this is the Purescript Tree after processing in JS to remove empty child fields from leaves etc
-- need to ensure that this structure is encapsulated in libraries (ie by moving this code)
foreign import data D3Tree :: Type
type D3TreeNode = {
    "data" :: ModelData -- guaranteed coercible to the `a` of the `Model a`
  , x :: Number
  , y :: Number
  , name :: String
  , value :: String
  , depth :: Number
  , height :: Number
-- these next too are guaranteed coercible to the same type, ie D3TreeNode
  , parent :: RecursiveD3TreeNode -- this won't be present in the root node
  , children :: Array RecursiveD3TreeNode -- this won't be present in leaf nodes
}

-- do the decode on the Purescript side unless files are ginormous, this is just for prototyping
foreign import readJSONJS :: forall a. String -> Tree String-- TODO no error handling at all here RN
foreign import d3Hierarchy :: forall a. Tree a -> D3Tree
foreign import hasChildren :: Datum -> Boolean

readModelFromFileContents :: forall r. Either Error { body ∷ String | r } -> Tree String
readModelFromFileContents (Right { body } ) = readJSONJS body
readModelFromFileContents (Left err)        = Node "error reading tree" []

-- we give the chart our Model type but behind the scenes it is mutated by D3 and additionally
-- which projection of the "Model" is active in each Join varies so we can't have both strong
-- static type representations AND lightweight syntax with JS compatible lambdas
d3TreeNode :: Datum -> D3TreeNode
d3TreeNode = unsafeCoerce

chart :: Tuple Number Number -> Selection (Model String)
chart (Tuple width height) = 
  let
    origin = { x: -width / 2.0, y: -height / 2.0 }
    -- three little transform functions to build up the transforms on nodes and labels
    rotate x = show $ (x * 180.0 / pi - 90.0)
    rotateCommon d = "rotate(" <> rotate (d3TreeNode d).x <> ")"
    rotateText2 d = "rotate(" <> if (d3TreeNode d).x >= pi then "180" else "0" <> ")"
    -- same translation for both text and node
    translate d = "translate(" <> show (d3TreeNode d).y <> ",0)"
  in
    initialSelect "div#tree" "treeLayout" noAttrs $ [
      appendNamed "svg" Svg [ viewBox origin.x origin.y width height ] [
        append Group noAttrs 
          [ join Path modelLinks
            (appendNamed "link" Path [ strokeWidth 1.5
                                     , strokeColor "#555"
                                     , strokeOpacity 0.4
                                     , fill "none"
                                     , radialLink (\d -> (d3TreeNode d).x) (\d -> (d3TreeNode d).y) ] noChildren)
            noUpdate noExit ]
          
        , append Group noAttrs
          [ join Circle modelDescendents
            (appendNamed "node" Circle [ radius 2.5
                                       , fill_D (\d -> if hasChildren d then "#555" else "#999")
                                       , transform [ rotateCommon, translate ] ] noChildren)
            noUpdate noExit ]
        , append Group [ fontFamily "sans-serif"
                       , fontSize 10.0
                       , strokeLineJoin Round
                       , strokeWidth 3.0]
          [ join Text modelDescendents
            (appendNamed "text" Text [ transform [ rotateCommon, translate, rotateText2]
                                     , StaticString "dy" "0.31em"
                                     , NumberAttr "x" labelOffset
                                     , StringAttr "text-anchor" textOffset
                                     , TextAttr (\d -> (d3TreeNode d).name) 
                                     -- TODO add clone step later 
                                     ] noChildren)
            noUpdate noExit ]
        ]
    ]




labelOffset :: Datum -> Number
labelOffset d =
  if ((d3TreeNode d).x < pi) == (hasChildren d)
  then 6.0
  else -6.0

textOffset :: Datum -> String
textOffset d =
  if ((d3TreeNode d).x < pi) == (hasChildren d)
  then "start"
  else "end"

-- Projection functions to get subModels out of the Model for sub-selections
foreign import d3HierarchyLinks :: D3Tree -> SubModel
foreign import d3HierarchyDescendents :: D3Tree -> SubModel

modelLinks :: forall a. Model a -> SubModel
modelLinks (Model model) = d3HierarchyLinks model.d3Tree

modelDescendents :: forall a. Model a -> SubModel
modelDescendents (Model model) = d3HierarchyDescendents model.d3Tree

