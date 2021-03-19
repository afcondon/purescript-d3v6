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

  | Append {
      element      :: Element
    , children     :: Array (Selection model)
    , attributes   :: Array Attr
  }
    
  | Join { -- enter with option update and/or exit enter will append the element, exit will terminate with remove
        element    :: Element
      , selections :: { enter      :: Selection model
                      , update     :: Maybe (Selection model)   -- would have to be an Update 
                      , exit       :: Maybe (Selection model) } -- would have to be an Exit
      , projection :: Maybe (Projection model)
  }

  | Update {
      children     :: Array (Selection model)
    , attributes   :: Array Attr
  }

  | Exit {
      children     :: Array (Selection model)
    , attributes   :: Array Attr
  }
  -- TODO can Transition, Call, Clone and other things be part of Selection?
  
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
 

data JoinParameters model = 
    AttrsAndChildren (Array Attr) (Array (Selection model))
  | AttrsAndChild    (Array Attr)        (Selection model)
  | Attrs            (Array Attr)
  | Children                      (Array (Selection model))
  | Child                                (Selection model)
  
data OnJoin model = 
    Enter           Element (JoinParameters model) 
  | EnterUpdate     Element (JoinParameters model) (JoinParameters model)
  | EnterExit       Element (JoinParameters model) (JoinParameters model)
  | EnterUpdateExit Element (JoinParameters model) (JoinParameters model) (JoinParameters model)

-- underlying function for all the enter variations
join_ :: forall model. Maybe (Projection model) -> OnJoin model -> Selection model
join_ projection =
  case _ of 
    (Enter element p) ->  
      Join { element
           , projection
           , selections: { enter:  makeEnterSelection element p
                         , update: Nothing
                         , exit:   Nothing }
      }

    (EnterUpdate element pe pu) -> 
      Join { element
           , projection
           , selections: { enter:  makeEnterSelection element pe
                         , update: Just (makeUpdateSelection pu)
                         , exit:   Nothing }
      }

    (EnterExit element pe px) -> 
      Join { element
           , projection
           , selections: { enter:  makeEnterSelection element pe
                         , update: Nothing
                         , exit:   Just (makeExitSelection px) }
      }

    (EnterUpdateExit element pe pu px) -> 
      Join { element
           , projection
           , selections: { enter:  makeEnterSelection element pe
                         , update: Just (makeUpdateSelection pu)
                         , exit:   Just (makeExitSelection px) }
      }


join :: forall model. OnJoin model -> Selection model
join onJoin = join_ Nothing onJoin

joinUsing :: forall model. Projection model -> OnJoin model -> Selection model
joinUsing projection onJoin = join_ (Just projection) onJoin

makeEnterSelection :: forall model. Element -> JoinParameters model -> Selection model
makeEnterSelection element =
  case _ of
    (AttrsAndChildren attributes children) -> Append { element, attributes, children }
    (AttrsAndChild attributes child)       -> Append { element, attributes, children: [child] }
    (Children children)                    -> Append { element, attributes: [], children }
    (Child child)                          -> Append { element, attributes: [], children: [child] }
    (Attrs attributes)                     -> Append { element, attributes, children: [] }

makeUpdateSelection :: forall model. JoinParameters model -> Selection model
makeUpdateSelection =
  case _ of
    (AttrsAndChildren attributes children) -> Update { attributes, children }
    (AttrsAndChild attributes child)       -> Update { attributes, children: [child] }
    (Children children)                    -> Update { attributes: [], children }
    (Child child)                          -> Update { attributes: [], children: [child] }
    (Attrs attributes)                     -> Update { attributes, children: [] }

makeExitSelection :: forall model. JoinParameters model -> Selection model
makeExitSelection =
  case _ of
    (AttrsAndChildren attributes children) -> Exit { attributes, children }
    (AttrsAndChild attributes child)       -> Exit { attributes, children: [child] }
    (Children children)                    -> Exit { attributes: [], children }
    (Child child)                          -> Exit { attributes: [], children: [child] }
    (Attrs attributes)                     -> Exit { attributes, children: [] }




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
