module D3.Base.Selection where

import D3.Base.Attributes (Attr)
import D3.Base.Foreign (SubModel)
import Prelude (class Show, ($), (<>))

import Data.Maybe (Maybe(..))
import Data.Array (singleton)

type Label = String
type Selector = String
data Element = Svg | Group | Div | Line | Circle | Path | Text | Table | Td | Tr

type Projection model = model -> SubModel

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
      element      :: Element
    , children     :: Array (Selection model)
    , attributes   :: Array Attr
  }
    
  | Join { -- enter with option update and/or exit enter will append the element, exit will terminate with remove
        element    :: Element
      , selections :: { enter      :: Selection model
                      , update     :: Selection model
                      , exit       :: Selection model }
      , projection :: Maybe (Projection model)
  }
  
  -- placeholder for a selection that isn't determined til runtime
  -- and which must be looked up 
  | RunTimeSelection String (Selection model)
  | NullSelection -- used for optional Join functions

selectInDOM :: forall model. Selector -> Array Attr -> Array (Selection model) -> Selection model 
selectInDOM selector attributes children = 
  InitialSelect { selector, attributes, children }

append :: forall model.        Element              -> Array Attr -> Array (Selection model) -> Selection model 
append element attributes children = 
  Append { element, attributes, children }

append_ :: forall model.       Element              -> Array Attr                            -> Selection model 
append_ element attributes = 
  Append { element, attributes, children: [] }

modifySelection :: forall model. String -> Selection model -> Selection model
modifySelection name b = RunTimeSelection name b
 
-- underlying function for all the enter variations
join :: forall model.
  Element ->
  Maybe (Projection model) -> -- projection function to present only the appropriate data to this join
  { enter  :: OnJoin model, update :: OnJoin model, exit :: OnJoin model } ->
  Selection model
join element projection protoSelections = 
  Join { element
       , selections: makeJoinSelections element protoSelections
       , projection }
        
joinEnter :: forall model. Element -> Projection model -> OnJoin model -> Selection model
joinEnter element projection enter =
  join element (Just projection) { enter, update: NoUpdate, exit: NoExit }

joinEnterAndUpdate :: forall model. Element -> Projection model -> OnJoin model -> OnJoin model -> Selection model
joinEnterAndUpdate element projection enter update =
  join element (Just projection) { enter, update, exit: NoExit }

joinEnterAndExit :: forall model. Element -> Projection model -> OnJoin model -> OnJoin model -> Selection model
joinEnterAndExit element projection enter exit =
  join element (Just projection) { enter, update: NoUpdate, exit }

joinEnterUpdateExit :: forall model. Element -> Projection model -> OnJoin model -> OnJoin model -> OnJoin model -> Selection model
joinEnterUpdateExit element projection enter update exit =
  join element (Just projection) { enter, update, exit }

joinEnter_ :: forall model. Element -> OnJoin model -> Selection model
joinEnter_ element enter =
  join element Nothing { enter, update: NoUpdate, exit: NoExit }

joinEnterAndUpdate_ :: forall model. Element -> OnJoin model -> OnJoin model -> Selection model
joinEnterAndUpdate_ element enter update =
  join element Nothing { enter, update, exit: NoExit }

joinEnterAndExit_ :: forall model. Element -> OnJoin model -> OnJoin model -> Selection model
joinEnterAndExit_ element enter exit =
  join element Nothing { enter, update: NoUpdate, exit }

joinEnterUpdateExit_ :: forall model. Element -> OnJoin model -> OnJoin model -> OnJoin model -> Selection model
joinEnterUpdateExit_ element enter update exit =
  join element Nothing { enter, update, exit }


data OnJoin model = -- essentially a Selection without Element
    Enter         (Array Attr) (Array (Selection model))
  | Update        (Array Attr) (Array (Selection model))
  | Exit          (Array Attr) (Array (Selection model))
  | EnterNoAttrs  (Array (Selection model))
  | UpdateNoAttrs (Array (Selection model))
  | ExitNoAttrs   (Array (Selection model))
  | EnterAttrs    (Array Attr)
  | UpdateAttrs   (Array Attr)
  | ExitAttrs     (Array Attr)
  | NoUpdate
  | NoExit

mkSelection :: forall model. Element -> OnJoin model -> Selection model
mkSelection element = case _ of
  (Enter attrs children)  -> append element attrs children -- TODO only enter is going to work here RN
  (Update attrs children) -> append element attrs children -- we're NOT going to append here
  (Exit attrs children)   -> append element attrs children -- and we're going to REMOVE here
  (EnterAttrs attrs)      -> append_ element attrs -- TODO only enter is going to work here RN
  (UpdateAttrs attrs)     -> append_ element attrs -- we're NOT going to append here
  (ExitAttrs attrs)       -> append_ element attrs -- and we're going to REMOVE here
  NoUpdate                -> NullSelection -- updates are optional
  NoExit                  -> NullSelection -- exits are optional

makeJoinSelections :: forall model. Element ->
  { enter  :: OnJoin model,   update :: OnJoin model,    exit   :: OnJoin model } ->
  { enter :: Selection model, update :: Selection model, exit :: Selection model }
makeJoinSelections element protos = { enter, update, exit }
  where
    enter  = mkSelection element protos.enter
    update = mkSelection element protos.update
    exit   = mkSelection element protos.exit

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
  show Table  = "table"
  show Td     = "td"
  show Tr     = "tr"
