module D3.Base.Selection where

import D3.Base.Attributes
import D3.Base.Foreign
import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (singleton)

type Label = String
type Selector = String
data Element = Svg | Group | Div | Line | Circle | Path | Text

type Projection model = model -> SubModel

-- | Types to represent Selection and Insertion
-- | you can append a list of many (different) elements 
-- | or an entire selection of only one type of element bound to some data
-- | Selection will i guess be an indexed monad? in order to move back and forth
-- | between Selection/Transition
data Selection model = 
    InitialSelect {
      label        :: Maybe String -- root should have a label so that we have a key for it in the map, but we will add it with nameSelection fn
    , selector     :: String
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
      projection   :: Maybe (Projection model) -- function that can extract submodel for subselection
    , element      :: Element -- has to agree with the Element in enter/update/exit
    , enterUpdateExit :: EnterUpdateExit model
  }
  | Transition {
      label        :: Maybe String
    , duration     :: Number
    , attributes   :: Array Attr
  }
  | NullSelection -- used for optional Join functions

type EnterUpdateExit model = {
    enter  :: Selection model
  , update :: Selection model
  , exit   :: Selection model
}

nameSelection :: forall model. Label -> Selection model -> Selection model
nameSelection label = 
  case _ of 
    (InitialSelect s) -> InitialSelect $ s { label = Just label }
    (Append s)        -> Append $ s { label = Just label }
    (Transition t)    -> Transition $ t { label = Just label }
    (Join j)          -> Join j
    NullSelection     -> NullSelection

selectInDOM :: forall model. Selector -> Array Attr -> Array (Selection model) -> Selection model 
selectInDOM selector attributes children = 
  InitialSelect { label: Nothing, selector, attributes, children }

append :: forall model.        Element           -> Array Attr -> Array (Selection model) -> Selection model 
append element attributes children = 
  Append { label: Nothing, element, attributes, children }

append_ :: forall model.       Element           -> Array Attr                            -> Selection model 
append_ element attributes = 
  Append { label: Nothing, element, attributes, children: [] }

-- add a Selection to the children field of another selection
extendSelection :: forall model. Selection model -> Selection model -> Selection model
extendSelection a b = 
  case a of
    (Append a) -> Append a { children = singleton b <> a.children}
    otherwise -> a -- TODO for now we'll just do nothing in other cases
    -- (selectInDOM i)
    -- (Join j)
    -- (Transition _)
    -- NullSelection

-- noUpdate       = NullSelection :: forall model. Selection model
-- noExit         = NullSelection :: forall model. Selection model
-- emptySelection = NullSelection :: forall model. Selection model

joinAllData :: forall model. 
     Element
  -> EnterUpdateExit model -- minimal definition requires only enter
  -> Selection model
joinAllData element enterUpdateExit = 
  Join { projection: Nothing, element, enterUpdateExit }

simpleJoinAllData :: forall model. 
     Element
  -> Selection model -- minimal definition requires only enter
  -> Selection model
simpleJoinAllData element enter = 
  Join { projection: Nothing, element, enterUpdateExit: { enter, update: NullSelection, exit: NullSelection } }
 
joinToSubset :: forall model. 
     Element
  -> Projection model-- projection function to present only the appropriate data to this join
  -> EnterUpdateExit model -- minimal definition requires only enter
  -> Selection model
joinToSubset element projection enterUpdateExit = 
  Join { projection: Just projection, element, enterUpdateExit }

simpleJoinSubSet:: forall model. 
     Element
  -> Projection model -- projection function to present only the appropriate data to this join
  -> Selection model  -- minimal definition requires only enter
  -> Selection model
simpleJoinSubSet element projection enter = 
  Join { projection: Just projection, element, enterUpdateExit: { enter, update: NullSelection, exit: NullSelection } }
 
infixl 1 joinAllData       as <-+->
infixl 1 simpleJoinAllData as <--->
infixl 1 joinToSubset      as <-/\->
infixl 1 simpleJoinSubSet  as <->


-- |              Show instance etc

instance showSelection :: Show (Selection a) where
  show (InitialSelect r) = "d3.selectAll(\"" <> r.selector <> "\")" <> " name: " <> show r.label <> " " <> show r.attributes <> show r.children
  show (Append r) = 
    let prefix = case r.label of
                  (Just name) -> "const " <> name <> " = "
                  Nothing -> "\n"
    in prefix <> ".append(\"" <> show r.element <> "\")" <> show r.attributes <> show r.children
  show (Join r) = "Join" <> 
                  show r.enterUpdateExit.enter <>
                  show r.enterUpdateExit.update <>
                  show r.enterUpdateExit.exit
  show (Transition r) = 
    let prefix = case r.label of
                  (Just name) -> "const " <> name <> " = "
                  Nothing -> "\n"
    in prefix <> ".transition(\"" <> show r.duration <> "\")" <> show r.attributes
  show NullSelection = ""

instance showElement :: Show Element where
  show Svg    = "svg"
  show Group  = "g"
  show Div    = "div"
  show Line   = "line"
  show Circle = "circle"
  show Path   = "path"
  show Text   = "text"
