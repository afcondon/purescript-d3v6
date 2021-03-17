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
  | Join String Element (Selection model) (Selection model) (Selection model) (Maybe (Projection model))
  
  | Transition {
      label        :: Maybe String
    , duration     :: Number
    , attributes   :: Array Attr
  }
  -- placeholder for a selection that isn't determined til runtime
  -- and which must be looked up 
  | RunTimeSelection String (Selection model)
  | NullSelection -- used for optional Join functions

data EnterUpdateExit model = EnterUpdateExit {
    enter  :: Element -> Selection model
  , update :: Element -> Selection model
  , exit   :: Element -> Selection model
}

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
  -> String
  -> Element
  -> EnterUpdateExit model 
  -> Selection model
join projection name element (EnterUpdateExit selections) = 
  Join name element
       (selections.enter element)
       (selections.update element)
       (selections.exit element)
       (Just projection)

-- version without projection function (more correctly projection = identity)
-- TODO rename join and join_
join_ :: forall model. 
     String
  -> Element
  -> EnterUpdateExit model 
  -> Selection model
join_ name element (EnterUpdateExit selections) = 
  Join name element
       (selections.enter element)
       (selections.update element)
       (selections.exit element)
       Nothing
 
infixl 1 join  as ==>

withAttributes :: forall model. Array Attr -> Element -> Selection model
withAttributes attrs = \e -> append_ e attrs
                            
enter :: forall model. 
  (Element -> Selection model) ->
  EnterUpdateExit model
enter s = EnterUpdateExit { enter: s, update: \_ -> NullSelection, exit: \_ -> NullSelection }

enterUpdate :: forall model. 
  { enter  :: (Element -> Selection model)
  , update :: (Element -> Selection model)
  } ->
  EnterUpdateExit model
enterUpdate eux = EnterUpdateExit { enter: eux.enter, update: eux.update, exit: \_ -> NullSelection }

enterExit :: forall model. 
  { enter  :: (Element -> Selection model)
  , exit   :: (Element -> Selection model)
  } ->
  EnterUpdateExit model
enterExit eux = EnterUpdateExit { enter: eux.enter, update: \_ -> NullSelection, exit: eux.exit }

enterUpdateExit :: forall model. 
  { enter  :: (Element -> Selection model)
  , update :: (Element -> Selection model)
  , exit   :: (Element -> Selection model)
  } ->
  EnterUpdateExit model
enterUpdateExit eux = EnterUpdateExit { enter: eux.enter, update: eux.update, exit: eux.exit }

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
