module D3.Base.Selection where

import D3.Base.Attributes (Attr)
import D3.Base.Foreign (SubModel)
import Prelude (class Show, ($), (<>))

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
      projection      :: Maybe (Projection model) -- function that can extract submodel for subselection
    , enterUpdateExit :: EnterUpdateExit model
  }
  | Transition {
      label        :: Maybe String
    , duration     :: Number
    , attributes   :: Array Attr
  }
  -- placeholder for a selection that isn't determined til runtime
  -- and which must be looked up 
  | RunTimeSelection String (Selection model)
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
    -- TODO exception if renaming attempted
    (RunTimeSelection n s) -> RunTimeSelection n s -- no renaming allowed

selectInDOM :: forall model. Selector -> Array Attr -> Array (Selection model) -> Selection model 
selectInDOM selector attributes children = 
  InitialSelect { label: Nothing, selector, attributes, children }

append :: forall model.        Element              -> Array Attr -> Array (Selection model) -> Selection model 
append element attributes children = 
  Append { label: Nothing, element, attributes, children }

append_ :: forall model.       Element              -> Array Attr                            -> Selection model 
append_ element attributes = 
  Append { label: Nothing, element, attributes, children: [] }

-- add a Selection to the children field of another selection
-- only works on Append selections for now
extendSelection :: forall model. Selection model -> Selection model -> Selection model
extendSelection s1 s2 = 
  case s1 of
    (Append a) -> Append a { children = singleton s2 <> a.children}
    otherwise -> s1 -- TODO for now we'll just do nothing in other cases

modifySelection :: forall model. String -> Selection model -> Selection model
modifySelection name b = RunTimeSelection name b
 
join :: forall model. 
     Projection model -- projection function to present only the appropriate data to this join
  -> EnterUpdateExit model
  -> Selection model
join projection eue = 
  Join { projection: Just projection, enterUpdateExit: eue }
 
infixl 1 join  as ==>

enter :: forall model. Selection model -> EnterUpdateExit model
enter s = { enter: s, update: NullSelection, exit: NullSelection }

enterUpdate :: forall model. Selection model -> Selection model -> EnterUpdateExit model
enterUpdate s u = { enter: s, update: u, exit: NullSelection }

enterExit :: forall model. Selection model -> Selection model -> EnterUpdateExit model
enterExit s e = { enter: s, update: NullSelection, exit: e }

enterUpdateExit :: forall model. Selection model -> Selection model -> Selection model -> EnterUpdateExit model
enterUpdateExit s u e = { enter: s, update: u, exit: NullSelection }

-- TODO rewrite the show instance once Selection ADT settles down
-- |              Show instance etc

-- instance showSelection :: Show (Selection a) where
--   show (InitialSelect r) = "d3.selectAll(\"" <> r.selector <> "\")" <> " name: " <> show r.label <> " " <> show r.attributes <> show r.children
--   show (Append r) = 
--     let prefix = case r.label of
--                   (Just name) -> "const " <> name <> " = "
--                   Nothing -> "\n"
--     in prefix <> ".append(\"" <> show r.element <> "\")" <> show r.attributes <> show r.children
--   show (Join r) = "Join" <> 
--                   show r.enterUpdateExit.enter <>
--                   show r.enterUpdateExit.update <>
--                   show r.enterUpdateExit.exit
--   show (Transition r) = 
--     let prefix = case r.label of
--                   (Just name) -> "const " <> name <> " = "
--                   Nothing -> "\n"
--     in prefix <> ".transition(\"" <> show r.duration <> "\")" <> show r.attributes
--   show NullSelection = ""
--   show (RunTimeSelection name selection) = 

instance showElement :: Show Element where
  show Svg    = "svg"
  show Group  = "g"
  show Div    = "div"
  show Line   = "line"
  show Circle = "circle"
  show Path   = "path"
  show Text   = "text"
