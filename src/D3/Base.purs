module D3.Base (
    selectInDOM, append, append_, appendAs, appendAs_
  , join, simpleJoin, transition, transitionAs, extendSelection
-- foreign (opaque) types for D3 things that are only needed as references on PureScript side
  , Datum, SubModel, NativeSelection, Scale
-- foreign functions exported by Base
  , d3SchemeCategory10JS
  , Attr(..), TickMap, DragBehavior(..)
  , Selection(..), EnterUpdateExit -- only exported here to build interpreter, will be hidden when code tidied up
  , Element(..)
  , Force(..), ForceType(..), Link, Node, IdFn, ID, Label, LineJoin(..)
  , Simulation(..), SimulationRecord, SimulationConfig, defaultConfigSimulation, SimulationNodeRow
  , noAttributes, radialLink, transform, TransformFn, fontFamily, fontSize, strokeLineJoin
  , strokeColor, strokeColor_D, strokeWidth, strokeWidth_D, strokeOpacity, strokeOpacity_D, radius, radius_D, fill, fill_D, viewBox
) where

import Prelude

import Data.Array (singleton)
import Data.Foldable (intercalate)
import Data.Map (Map)
import Data.Maybe (Maybe(..))

-- | these foreign types allow us to work with some very funky typing without 
-- | adding tonnes of syntactic noise or type complexity
-- | NativeSelection is an opaque type which we keep in order to feed it back to D3 
-- | when we look up named Selections, Transitions, Simulations, whatever
foreign import data NativeSelection :: Type
-- | The Datum models the (variable / polymorphic) type of the lambdas used in Attr
foreign import data Datum       :: Type
-- | The SubModel is that portion of the Model that we give to a particular Join
foreign import data SubModel    :: Type
type Scale = Number -> String
foreign import d3SchemeCategory10JS :: Scale -- not modelling the scale / domain distinction yet

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
-- this is the row that gets added ot your Model's nodes when initialized by D3
type SimulationNodeRow r = { x :: Number, y :: Number, group :: Number, vx :: Number, vy :: Number, index :: Number }

-- | Force Layout core types
type ID = Int -- TODO this needs to be polymorphic eventually
type Link = forall r. { id :: ID, source :: ID, target :: ID | r }
type Node = forall r i. { id :: i | r }
type IdFn = Link -> ID
data Force = Force Label ForceType
data ForceType =
    ForceMany
  | ForceCenter Number Number
  -- | ForceLink (Array Link) IdFn
  | ForceCollide Number
  | ForceX Number
  | ForceY Number
  | ForceRadial Number Number
  | Custom

type SimulationRecord = { 
    label  :: String
  , config :: SimulationConfig
  , nodes  :: Array Node
  , links  :: Array Link
  , forces :: Array Force
  , tick   :: Unit -> Unit -- could be Effect Unit
  , drag   :: Simulation -> Unit -- could be Effect Unit
}
data Simulation = Simulation SimulationRecord
type TickMap model = Map String (Array Attr)
data DragBehavior = DefaultDrag String String -- only one implementation rn and implemented on JS side 

-- | Types to represent Selection and Insertion
-- | you can append a list of many (different) elements 
-- | or an entire selection of only one type of element bound to some data
-- | Selection will i guess be an indexed monad? in order to move back and forth
-- | between Selection/Transition
data Selection model = 
    InitialSelect {
      label        :: String -- root must have a label so that we have a key for it in the map
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
      projection   :: model -> SubModel -- function that can extract submodel for subselection
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

selectInDOM :: forall model. Selector -> Label -> Array Attr -> Array (Selection model) -> Selection model 
selectInDOM selector label attributes children = 
  InitialSelect { label, selector, attributes, children }

append :: forall model.        Element           -> Array Attr -> Array (Selection model) -> Selection model 
append element attributes children = 
  Append { label: Nothing, element, attributes, children }

append_ :: forall model.       Element           -> Array Attr                            -> Selection model 
append_ element attributes = 
  Append { label: Nothing, element, attributes, children: [] }

appendAs :: forall model. Label -> Element -> Array Attr -> Array (Selection model) -> Selection model 
appendAs label element attributes children = 
  Append { label: Just label, element, attributes, children }

appendAs_ :: forall model. Label -> Element -> Array Attr ->                           Selection model 
appendAs_ label element attributes = 
  Append { label: Just label, element, attributes, children: [] }


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

join :: forall model. Element
    -> (model -> SubModel) -- projection function to present only the appropriate data to this join
    -> EnterUpdateExit model -- minimal definition requires only enter
    -> Selection model
join element projection enterUpdateExit = 
  Join { projection, element, enterUpdateExit }

simpleJoin :: forall model. Element
    -> (model -> SubModel) -- projection function to present only the appropriate data to this join
    -> Selection model -- minimal definition requires only enter
    -> Selection model
simpleJoin element projection enter = 
  Join { projection, element, enterUpdateExit: { enter, update: NullSelection, exit: NullSelection } }

transition :: forall model. Number -> Array Attr -> Selection model 
transition duration attributes = 
  Transition { label: Nothing, duration, attributes }

transitionAs :: forall model. Label -> Number -> Array Attr -> Selection model 
transitionAs label duration attributes = 
  Transition { label: Just label, duration, attributes }

-- internal definitions of Attrs, this is what the interpreter will work with
data Attr =
-- first the direct, static attributes
    StaticString    String String
  | StaticNumber    String Number
  | StaticArrayNumber String (Array Number)
-- then the ones that are function of Datum only
  | StringAttr      String (Datum -> String)
  | NumberAttr      String (Datum -> Number)
  | ArrayNumberAttr String (Datum -> Array Number)
-- lastly attribute functions that take Datum and the index
  | StringAttrI      String (Datum -> Number -> String)
  | NumberAttrI      String (Datum -> Number -> Number)
  | ArrayNumberAttrI String (Datum -> Number -> Array Number)
-- Text in D3 is not an attribute but syntactically and semantically it really is
-- so in our DSL we will just make it one and hide that fact
  | TextAttr        (Datum -> String)
-- prettier definitions for attributes
noAttributes :: Array Attr
noAttributes = []

strokeColor :: String -> Attr
strokeColor = StaticString "stroke"

strokeColor_D :: (Datum -> String) -> Attr
strokeColor_D = StringAttr "stroke"

strokeWidth :: Number -> Attr
strokeWidth = StaticNumber "stroke-width"

strokeWidth_D :: (Datum -> Number) -> Attr
strokeWidth_D = NumberAttr "stroke-width"
  
strokeOpacity :: Number -> Attr
strokeOpacity = StaticNumber "stroke-opacity"

strokeOpacity_D :: (Datum -> Number) -> Attr
strokeOpacity_D = NumberAttr "stroke-opacity"

radius :: Number -> Attr
radius = StaticNumber "r"

radius_D :: (Datum -> Number) -> Attr
radius_D = NumberAttr "r"

fill :: String -> Attr
fill = StaticString "fill"

fill_D :: (Datum -> String) -> Attr
fill_D = StringAttr "fill"

viewBox :: Number -> Number -> Number -> Number -> Attr
viewBox xo yo width height = StaticArrayNumber "viewBox" [ xo, yo, width, height ]

fontFamily :: String -> Attr
fontFamily = StaticString "font-family"
  
fontSize :: Number -> Attr
fontSize = StaticNumber "font-size"

data LineJoin = Arcs | Bevel | Miter | MiterClip | Round
instance showLineJoin :: Show LineJoin where
  show Arcs = "arcs"
  show Bevel = "bevel"
  show Miter = "miter"
  show MiterClip = "miter-clip"
  show Round = "round"

strokeLineJoin :: LineJoin -> Attr
strokeLineJoin linejoin = StaticString "stroke-linejoin" $ show linejoin
    
foreign import d3LinkRadial :: (Datum -> Number) -> (Datum -> Number) -> (Datum -> String)
radialLink :: (Datum -> Number) -> (Datum -> Number) -> Attr
radialLink angleFn radiusFn = StringAttr "d" $ d3LinkRadial angleFn radiusFn

type TransformFn = Datum -> String
transform :: Array TransformFn -> Attr
transform fs = StringAttr "transform" (\d -> showTransform d)
  where
    showTransform d = intercalate " " $ flap fs d

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

enQuote :: String -> String
enQuote string = "\"" <> string <> "\""

instance showAttribute :: Show Attr where
  show (StaticString a v) = "\n.attr(\"" <> a <> "\", \"" <> v <> "\")"
  show (StaticNumber a v) = "\n.attr(\"" <> a <> "\", \"" <> show v <> "\")"
  show (StaticArrayNumber a v) = "\n.attr(\"" <> a <> "\", \"" <> show v <> "\")"
  show (StringAttr a fn) = "\n.attr(\"" <> a <> "\", <\\d -> result>)"
  show (TextAttr fn) = "\n.attr(\"" <> "Text" <> "\", <\\d -> result>)"
  show (NumberAttr a fn) = "\n.attr(\"" <> a <> "\", <\\d -> result>)"
  show (ArrayNumberAttr a fn) = "\n.attr(\"" <> a <> "\", <\\d -> result>)"
  show (StringAttrI a fn) = "\n.attr(\"" <> a <> "\", <\\d i -> result>)"
  show (NumberAttrI a fn) = "\n.attr(\"" <> a <> "\", <\\d i -> result>)"
  show (ArrayNumberAttrI a fn) = "\n.attr(\"" <> a <> "\", <\\d i -> result>)"

instance showElement :: Show Element where
  show Svg    = "svg"
  show Group  = "g"
  show Div    = "div"
  show Line   = "line"
  show Circle = "circle"
  show Path   = "path"
  show Text   = "text"
