module D3.Selection
  ( Selection         -- Types
  , CallbackParam
  , CallbackParamP
  , d3Select
  , d3SelectAll
  , append
  , listOfAttr
  , getAttr   -- getters provided separate because they don't return a Selection
  -- , call   -- TBD
  , call, call1, call2, call3, call4, call5, call6, call7, call8, call9
  , dataBindArray
  , dataBindIndexArray
  , dataBindHierarchy
  , dataBindIndexHierarchy
  -- , each   -- TBD
  , empty
  , enter
  , exit
  , insert
  , merge
  , node
  , nodes
  , on
  , on'             -- revisit this and see if can be wrapped in ADT like the other polymorphic functions TODO
  , remove
  , select
  , selectAll
  , selectElem
  , size
  , transition
  ) where

import Effect
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn5)
import D3.Base (D3Element, Hierarchy, Index)
import Web.Event.Event (EventType)
import Data.List (List(..), foldM)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe, Nullable)
import Prelude (Unit, bind, pure, (<$>))
import TaglessD3.AttrNew (Attr(..), getTag)

foreign import data Selection :: Type -> Type

-- missing SelectFnFn which takes a predicate fn to perform the selection
foreign import appendFn      :: ∀ d.          EffectFn2 String                      (Selection d) (Selection d)
foreign import bindDataFn    :: ∀ d1 d2.      EffectFn2 (Array d2)                  (Selection d1) (Selection d2)
foreign import bindDataFnK   :: ∀ d1 d2 k.    EffectFn3 (Array d2) (d2 -> k)        (Selection d1) (Selection d2)
foreign import bindHierarchyFn  :: ∀ d1 d2.   EffectFn2 (Hierarchy d2)              (Selection d1) (Selection d2)
foreign import bindHierarchyFnK :: ∀ d1 d2 k. EffectFn3 (Hierarchy d2) (d2 -> k)    (Selection d1) (Selection d2)
foreign import d3SelectAllFn :: ∀ d.          EffectFn1 String                                     (Selection d)
foreign import d3SelectFn    :: ∀ d.          EffectFn1 String                                     (Selection d)
foreign import emptyFn       :: ∀ d.          EffectFn1                             (Selection d)  Boolean
foreign import enterFn       :: ∀ d.          EffectFn1                             (Selection d) (Selection d)
foreign import exitFn        :: ∀ d.          EffectFn1                             (Selection d) (Selection d)
foreign import filterFn      :: ∀ d.          EffectFn2 String                      (Selection d) (Selection d)
foreign import filterFnP     :: ∀ d.          EffectFn2 (d -> Boolean)              (Selection d) (Selection d)
foreign import getAttrFn     :: ∀ v d.        EffectFn2 String                      (Selection d) v
foreign import insertFn      :: ∀ d.          EffectFn2 String                      (Selection d) (Selection d)
foreign import mergeFn       :: ∀ d.          EffectFn2 (Selection d)               (Selection d) (Selection d)
foreign import transitionFn  :: ∀ d.          EffectFn2 (Selection d)               (Selection d) (Selection d)
foreign import nodeFn        :: ∀ d.          EffectFn1                             (Selection d) (Nullable D3Element)
foreign import nodesFn       :: ∀ d.          EffectFn1                             (Selection d) (Array D3Element)
foreign import orderFn       :: ∀ d.          EffectFn1                             (Selection d) (Selection d)
foreign import removeFn      :: ∀ d.          EffectFn1                             (Selection d) (Selection d)
foreign import selectAllFn   :: ∀ d.          EffectFn2 String                      (Selection d) (Selection d)
foreign import selectElFn    :: ∀ d.          EffectFn1 D3Element                                 (Selection d) -- is this really in D3? TODO
foreign import selectFn      :: ∀ d.          EffectFn2 String                      (Selection d) (Selection d)
foreign import sizeFn        :: ∀ d.          EffectFn1                             (Selection d) Int

type D3CallbackDINE d r = (EffectFn4
                           d                 -- 1st arg of callback
                           Number            -- 2nd arg of callback
                           (Array D3Element) -- 3rd arg of callback
                           D3Element         -- 4th arg of callback
                           r)                -- result of callback

type D3CallbackText v  = (EffectFn2
                         v              -- 1st arg of callback
                         Index          -- 2nd arg of callback
                         String)        -- result of callback, a style


-- these foreign functions come in two flavors, simple versions and callback versions
foreign import classedFn  :: ∀ d. EffectFn3 String Boolean (Selection d) (Selection d)
foreign import classedFnP :: ∀ d. EffectFn3
                                  String               -- 1st arg of classedFnP
                                  (D3CallbackDINE d Boolean)  -- 2nd arg is callback
                                  (Selection d)             -- 3rd arg
                                  (Selection d)             -- result

foreign import styleFn :: ∀ d v. EffectFn3 String v (Selection d) (Selection d)
foreign import attrFn  :: ∀ d v. EffectFn3 String v (Selection d) (Selection d)

-- | turrrrrns out this is a Nullable value TODO
getAttr :: ∀ v d. String -> Selection d -> Effect v
getAttr s = runEffectFn2 getAttrFn  s

listOfAttr :: ∀ d. List Attr -> Selection d -> Effect (Selection d)
listOfAttr Nil s = pure s
listOfAttr as s = do
    s' <- arrayOfSelections
    pure s'
    where
        arrayOfSelections = foldM applyAttr s as
        applyAttr :: Selection d -> Attr -> Effect (Selection d)
        applyAttr sel at =
            case at of
            (Style stylename _) -> runEffectFn3 styleFn stylename at sel
            _ ->  runEffectFn3 attrFn (getTag at) at sel

d3Select :: ∀ d. String -> Effect (Selection d)
d3Select selector = runEffectFn1 d3SelectFn selector

d3SelectAll :: ∀ d. String -> Effect (Selection d)
d3SelectAll selector = runEffectFn1 d3SelectAllFn selector

selectAll :: ∀ d. String -> Selection d -> Effect (Selection d)
selectAll selector = runEffectFn2 selectAllFn selector

selectElem :: ∀ d. D3Element -> Effect (Selection d)  -- think this doesn't actually exist...TODO
selectElem element = runEffectFn1 selectElFn element

select  :: ∀ d.  String -> Selection d -> Effect (Selection d)
select selector = runEffectFn2 selectFn selector

-- would be nice to express that Keyed needs k to be Ord, will have to wait for GADTs
-- dataBind :: ∀ d k. Ord k => DataBind d k    -> Selection d -> Effect (Selection d)
dataBindArray :: forall d2 d1. Array d2 -> Selection d1 -> Effect (Selection d2)
dataBindArray d         = runEffectFn2 bindDataFn d

dataBindIndexArray :: ∀ d1 d2 k. Array d2 -> (d2 -> k) -> Selection d1 -> Effect (Selection d2)
dataBindIndexArray d keyFn   = runEffectFn3 bindDataFnK d keyFn

dataBindHierarchy :: ∀ d1 d2. Hierarchy d2           -> Selection d1 -> Effect (Selection d2)
dataBindHierarchy h         = runEffectFn2 bindHierarchyFn h

dataBindIndexHierarchy :: ∀ d1 d2 k. Hierarchy d2 -> (d2 -> k)            -> Selection d1 -> Effect (Selection d2)
dataBindIndexHierarchy h keyFn  = runEffectFn3 bindHierarchyFnK h keyFn

-- filter  :: ∀ d.  Filter d                   -> Selection d -> Effect (Selection d)
-- filter (Selector s)       = runEffectFn2 filterFn s
-- filter (Predicate p)      = runEffectFn2 filterFnP p

order :: ∀ d. Selection d -> Effect (Selection d)
order = runEffectFn1 orderFn

enter :: ∀ d. Selection d -> Effect (Selection d)
enter = runEffectFn1 enterFn

merge :: ∀ d. Selection d                -> Selection d -> Effect (Selection d)
merge = runEffectFn2 mergeFn

transition :: ∀ d. Selection d                -> Selection d -> Effect (Selection d)
transition = runEffectFn2 transitionFn

empty :: ∀ d. Selection d -> Effect Boolean
empty = runEffectFn1 emptyFn

node :: ∀ d. Selection d -> Effect (Maybe D3Element)
node s = toMaybe <$> runEffectFn1 nodeFn s

nodes :: ∀ d. Selection d -> Effect (Array D3Element)
nodes = runEffectFn1 nodesFn

exit :: ∀ d. Selection d -> Effect (Selection d)
exit = runEffectFn1 exitFn

remove :: ∀ d. Selection d -> Effect (Selection d)  -- maybe this should be Void? TODO
remove = runEffectFn1 removeFn

size :: ∀ d. Selection d -> Effect Int
size = runEffectFn1 sizeFn

insert  :: ∀ d.  String -> Selection d -> Effect (Selection d)
insert tag = runEffectFn2 insertFn tag

append  :: ∀ d.  String -> Selection d -> Effect (Selection d)
append tag = runEffectFn2 appendFn tag

-- using the slightly clunkier syntax of fn(selection, p1, p2) to mirror d3 syntax
call   :: ∀ x.
              (Selection x -> Effect(Selection x))
            -> Selection x -> Effect (Selection x)
call fn s =  fn s

call1   :: ∀ x a.
            (Selection x -> a -> Effect(Selection x))
            -> a
            -> Selection x -> Effect (Selection x)
call1 fn a s =  fn s a

call2   :: ∀ x a b.
            (Selection x -> a -> b -> Effect(Selection x))
            -> a -> b
            -> Selection x -> Effect (Selection x)
call2 fn a b s =  fn s a b

call3   :: ∀ x a b c.
            (Selection x -> a -> b -> c -> Effect(Selection x))
            -> a -> b -> c
            -> Selection x -> Effect (Selection x)
call3 fn a b c s =  fn s a b c

call4   :: ∀ x a b c d.
            (Selection x -> a -> b -> c -> d -> Effect(Selection x))
            -> a -> b -> c -> d
            -> Selection x -> Effect (Selection x)
call4 fn a b c d s =  fn s a b c d

call5   :: ∀ x a b c d e.
            (Selection x -> a -> b -> c -> d -> e -> Effect(Selection x))
            -> a -> b -> c -> d -> e
            -> Selection x -> Effect (Selection x)
call5 fn a b c d e s =  fn s a b c d e

call6   :: ∀ x a b c d e f.
            (Selection x -> a -> b -> c -> d -> e -> f -> Effect(Selection x))
            -> a -> b -> c -> d -> e -> f
            -> Selection x -> Effect (Selection x)
call6 fn a b c d e f s =  fn s a b c d e f

call7   :: ∀ x a b c d e f g.
            (Selection x -> a -> b -> c -> d -> e -> f -> g -> Effect(Selection x))
            -> a -> b -> c -> d -> e -> f -> g
            -> Selection x -> Effect (Selection x)
call7 fn a b c d e f g s =  fn s a b c d e f g

call8   :: ∀ x a b c d e f g h.
            (Selection x -> a -> b -> c -> d -> e -> f -> g -> h -> Effect(Selection x))
            -> a -> b -> c -> d -> e -> f -> g -> h
            -> Selection x -> Effect (Selection x)
call8 fn a b c d e f g h s =  fn s a b c d e f g h

call9   :: ∀ x a b c d e f g h i.
            (Selection x -> a -> b -> c -> d -> e -> f -> g -> h -> i -> Effect(Selection x))
            -> a -> b -> c -> d -> e -> f -> g -> h -> i
            -> Selection x -> Effect (Selection x)
call9 fn a b c d e f g h i s =  fn s a b c d e f g h i

-- || Callback stuff
-- first up from Graphics.D3.EffectFnExtra
type PropertyName = String
type CallbackParam d =
    { datum     :: d
    , elem      :: D3Element
    , timestamp :: Number
    , meta      :: Boolean
    , shift     :: Boolean
    , ctrl      :: Boolean
    , alt       :: Boolean
  }
type CallbackParamP d p =
    { datum     :: d
    , elem      :: D3Element
    , timestamp :: Number
    , prop      :: p
    , meta      :: Boolean
    , shift     :: Boolean
    , ctrl      :: Boolean
    , alt       :: Boolean
  }

foreign import data D3EffCallback      :: Type -> Type -> Type
foreign import data D3EffCallbackP     :: Type -> Type -> Type -> Type
foreign import mkCallback :: ∀ d r. (CallbackParam d -> Effect r) -> D3EffCallback (CallbackParam d) r
foreign import mkCallbackWithProp :: ∀ d p r. (CallbackParamP d p -> Effect r) -> PropertyName -> D3EffCallbackP (CallbackParamP d p) PropertyName r

foreign import onFn :: ∀ a d.
    EffectFn3
      (Selection a)       -- 1st argument for EffectFn3, the selection itself
      EventType           -- 2nd argument for EffectFn3, the type of the event being bound
      (D3EffCallback      -- 3rd argument for EffectFn3, this is the callback function
        (CallbackParam d) -- arg for callback mkCallback
        Unit)             -- result of mkCallback
      (Selection a)       -- result of EffectFn3, returns selection for "fluid interface" / monadic chain

foreign import onFnWithProperty :: ∀ a d p.
  EffectFn5
        (Selection a)               -- 1st argument for EffectFn5, the selection itself
        EventType                   -- 2nd argument for EffectFn5, the type of the event being bound
        (D3EffCallbackP -- 3rd argument for EffectFn5, this is the callback function
            (CallbackParamP d p)      -- arg 1 for callback mkCallbackWithProp,
            PropertyName              -- arg 2 for callback mkCallbackWithProp
            Unit)                     --  result of mkCallbackWithProp
        PropertyName                -- 4th argument for EffectFn5, name of a property to cache something in
        p                           -- 5th argument for EffectFn5, something to cache in the property field
        (Selection a)               -- result of EffectFn5, returns selection for "fluid interface" / monadic chain

-- generic "on" function works for any DOM event
on :: ∀ a d. EventType
         -> (CallbackParam d -> Effect Unit)
         -> (Selection a) -> Effect (Selection a)
on event callback selection  = runEffectFn3 onFn selection event (mkCallback callback)

on' :: ∀ a d p. EventType -> PropertyName -> p
         -> (CallbackParamP d p -> Effect Unit)
         -> (Selection a) -> Effect (Selection a)
on' evType propName prop callback sel = runEffectFn5 onFnWithProperty sel evType (mkCallbackWithProp callback propName) propName prop
