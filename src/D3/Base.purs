module D3.Base (
    select, append, appendNamed, join, transition, transitionNamed
  , Datum, SubModel
  , Attr(..)
  , Selection, Element(..), noUpdate, noExit, emptySelection
  , Force(..), ForceType(..), Link, IdFn, ID, Label
  , Simulation(..), SimulationConfig, defaultConfigSimulation
) where

import Prelude
import Data.Maybe(Maybe(..))

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
    , enter        :: Selection model
    , update       :: Selection model
    , exit         :: Selection model
  }
  | Transition {
      label        :: Maybe String
    , duration     :: Number
    , attributes   :: Array Attr
  }

  | NullSelection -- used for optional Join functions

select :: forall model. Selector -> Array Attr -> Array (Selection model) -> Selection model 
select selector attributes children = 
  InitialSelect { selector, attributes, children }

append :: forall model. Element -> Array Attr -> Array (Selection model) -> Selection model 
append element attributes children = 
  Append { label: Nothing, element, attributes, children }

appendNamed :: forall model. Label -> Element -> Array Attr -> Array (Selection model) -> Selection model 
appendNamed label element attributes children = 
  Append { label: Just label, element, attributes, children }

noUpdate = NullSelection
noExit   = NullSelection
emptySelection = NullSelection

join :: forall model. (model -> SubModel)
    -> Selection model -- minimal definition requires only enter
    -> Selection model -- update is optional
    -> Selection model -- exit is optional
    -> Selection model
join projection enter update exit = Join { "data": projection, enter, update, exit }

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
  | StaticString    String String
  | StaticNumber    String Number
  | StaticArrayNumber String (Array Number)

instance showSelection :: Show (Selection a) where
  show (InitialSelect r) = "d3.selectAll(\"" <> r.selector <> "\")" <> show r.attributes <> show r.children
  show (Append r) = 
    let prefix = case r.label of
                  (Just name) -> "const " <> name <> " = "
                  Nothing -> "\n"
    in prefix <> ".append(\"" <> show r.element <> "\")" <> show r.attributes <> show r.children
  show (Join r) = "Join" <> 
                  show r.enter <>
                  show r.update <>
                  show r.exit
  show (Transition r) = 
    let prefix = case r.label of
                  (Just name) -> "const " <> name <> " = "
                  Nothing -> "\n"
    in prefix <> ".transition(\"" <> show r.duration <> "\")" <> show r.attributes
  show NullSelection = ""

enQuote :: String -> String
enQuote string = "\"" <> string <> "\""

instance showAttribute :: Show Attr where
  show (StringAttr a fn) = "\n.attr(\"" <> a <> "\", <function>)"
  show (NumberAttr a fn) = "\n.attr(\"" <> a <> "\", <function>)"
  show (ArrayNumberAttr a fn) = "\n.attr(\"" <> a <> "\", <function>)"
  show (StaticString a v) = "\n.attr(\"" <> a <> "\", \"" <> v <> "\")"
  show (StaticNumber a v) = "\n.attr(\"" <> a <> "\", \"" <> show v <> "\")"
  show (StaticArrayNumber a v) = "\n.attr(\"" <> a <> "\", \"" <> show v <> "\")"

instance showElement :: Show Element where
  show Svg = "svg"
  show Group = "g"
  show Div = "div"
  show Line = "line"
  show Circle = "circle"
  show Path = "path"
  show Text = "text"