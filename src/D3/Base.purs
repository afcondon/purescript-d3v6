module D3.Base (
    select, append, appendNamed, join, transition, transitionNamed
  , Datum, SubModel
  , Attr(..), staticArrayNumberAttr, staticNumberAttr, staticStringAttr
  , Selection, Element(..)
  , Force(..), ForceType(..), Link, IdFn, ID, Label
  , Simulation(..), SimulationConfig, defaultConfigSimulation
) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)

-- | these foreign types allow us to work with some very funky typing without 
-- | adding tonnes of syntactic noise or type complexity
-- | The Datum models the (variable / polymorphic) type of the lambdas used in Attr
foreign import data Datum    :: Type
-- | The SubModel is that portion of the Model that we give to a particular Join
foreign import data SubModel :: Type

type Label = String
type Selector = String
data Element = Svg | Group | Div | Line | Circle | Path | Text

-- | a record to initialize / configure simulations
type SimulationConfig = { 
      alpha         :: Number
    , alphaTarget   :: Number
    , alphaMin      :: Number
    , alphaDecay    :: Number
    , velocityDecay :: Number
}
defaultConfigSimulation :: SimulationConfig
defaultConfigSimulation = { 
      alpha        : 1.0
    , alphaTarget  : 0.0
    , alphaMin     : 0.0001
    , alphaDecay   : 0.0228
    , velocityDecay: 0.4
}

-- | Force Layout core types
type ID = Int -- TODO this needs to be polymorphic eventually
type Link = forall r. { id :: ID, source :: ID, target :: ID | r }
type IdFn = Link -> ID
data Force = Force Label ForceType
data ForceType =
    ForceMany 
  | ForceCenter Number Number
  | ForceLink (Array Link) IdFn
  | ForceCollide
  | ForceX
  | ForceY
  | ForceRadial
  | Custom

data Simulation = Simulation { 
    label  :: String
  , config :: SimulationConfig
  , forces :: Array Force
  , tick   :: Unit -> Unit -- could be Effect Unit
  , drag   :: Simulation -> Unit -- could be Effect Unit
}
-- | Types to represent Selection and Insertion
-- | you can append a list of many (different) elements 
-- | or an entire selection of only one type of element bound to some data
-- | Selection will i guess be an indexed monad? in order to move back and forth
-- | between Selection/Transition
data Selection model = 
    InitialSelect {
      selector     :: String
    , attributes   :: Array Attr
    , children     :: Array (Selection model)
    }
  -- d3.selectAll, initial selection, possibly labelled so that it can be used elsewhere
  | Append {
      label        :: Maybe String
    , element      :: Element
    , attributes   :: Array Attr
    , children     :: Array (Selection model)
  }
  -- d3.selectAll().data().join() pattern
  | Join {
      "data"       :: model -> SubModel
    , enter        :: Selection model -> Unit
    , update       :: Selection model -> Unit
    , exit         :: Selection model -> Unit
  }
  | Transition {
      label        :: Maybe String
    , duration     :: Number
    , attributes   :: Array Attr
  }

select :: forall model. Selector -> Array Attr -> Array (Selection model) -> Selection model 
select selector attributes children = 
  InitialSelect { selector, attributes, children }

append :: forall model. Element -> Array Attr -> Array (Selection model) -> Selection model 
append element attributes children = 
  Append { label: Nothing, element, attributes, children }

appendNamed :: forall model. Label -> Element -> Array Attr -> Array (Selection model) -> Selection model 
appendNamed label element attributes children = 
  Append { label: Just label, element, attributes, children }

join :: forall model. (model -> SubModel)
    ->       (Selection model -> Unit) -- minimal definition requires only enter
    -> Maybe (Selection model -> Unit) -- update is optional
    -> Maybe (Selection model -> Unit) -- exit is optional
    -> Selection model
join projection enter maybeUpdate maybeExit = Join { "data": projection, enter, update, exit }
  where
    -- the optional update and exit can simply be no-ops for now
    update = fromMaybe (const unit) maybeUpdate
    exit   = fromMaybe (const unit) maybeExit

transition :: forall model. Number -> Array Attr -> Selection model 
transition duration attributes = 
  Transition { label: Nothing, duration, attributes }

transitionNamed :: forall model. Label -> Number -> Array Attr -> Selection model 
transitionNamed label duration attributes = 
  Transition { label: Just label, duration, attributes }

data Attr =
    StringAttr      String (Datum -> Number -> String)
  | NumberAttr      String (Datum -> Number -> Number)
  | ArrayNumberAttr String (Datum -> Number -> Array Number)

 -- just discard datum and index for now in these cases
 -- TODO suboptimal, we actually want to detect static attrs and hoist
 -- to parent so as not to pollute the DOM with a million duplicate attrs
staticStringAttr :: String -> String -> Attr
staticStringAttr name string = StringAttr name (\_ _ -> string)

staticNumberAttr :: String -> Number -> Attr
staticNumberAttr name number = NumberAttr name (\_ _-> number) -- just discard datum and index

staticArrayNumberAttr :: String -> Array Number -> Attr
staticArrayNumberAttr name numbers = ArrayNumberAttr name (\_ _-> numbers) -- just discard datum and index



