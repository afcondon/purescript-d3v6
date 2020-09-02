module D3.Transition
  ( D3DelayFn
  , TimeSpec(..)
  , AttrInterpolator(..)
  -- , (Selection d)
  , D3Transition(..)
  , d3Transition
  -- , addTransition
  , tAttr
  -- , call
  , delay
  , duration
  -- , each
  -- , ease
  , tEmpty
  , tFilter
  , tMerge
  , addTransition
  , makeTransition
  , namedTransition
  , savedTransition
  , tNode       -- implements D3 transition.node()
  , tNodes      -- implements D3 transition.nodes()
  -- , on
  , tRemove
  , tSelect
  , tSelectAll
  , makeSelection
  , tSize
  , tStyle      -- implements D3 transition.style() & D3 transition.styleTween
  -- , tText
  -- , tween
  ) where

-- || NB. These next functions are not implemented as separate functions - they're rolled up into tAttr and tStyle
  -- , attr
  -- , attrTween    -- rolled up (together with attr, style and styleTween) into tAttr & tStyle
  -- , style
  -- , styleTween

import D3.Base (D3Element, Filter(..))
import D3.Interpolator (Time, D3TweenFn, D3TweenTarget, Index)
import D3.Selection (Selection)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe, Nullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Prelude (class Show, show, (<$>), (<>))

foreign import d3TransitionFn    :: ∀ d.   EffectFn1 String (Selection d)
foreign import d3TransitionFn2   :: ∀ d.   Effect (Selection d)

foreign import attrFn            :: ∀ d v. EffectFn3 String v        (Selection d) (Selection d)
foreign import durationFn        :: ∀ d.   EffectFn2 Time            (Selection d) (Selection d)
foreign import emptyFn           :: ∀ d.   EffectFn1                 (Selection d) Boolean
foreign import mergeFn           :: ∀ d.   EffectFn2 (Selection d)   (Selection d) (Selection d)
foreign import namedTransitionFn :: ∀ d.   EffectFn2 String          (Selection d)  (Selection d)    -- changed from (s d) (t d)
foreign import nodeFn            :: ∀ d.   EffectFn1                 (Selection d) (Nullable D3Element)
foreign import nodesFn           :: ∀ d.   EffectFn1                 (Selection d) (Array D3Element)
foreign import styleFn           :: ∀ d v. EffectFn3 String v        (Selection d) (Selection d)
foreign import filterFn          :: ∀ d.   EffectFn2 String          (Selection d) (Selection d)
foreign import filterFnP         :: ∀ d.   EffectFn2 (d -> Boolean)  (Selection d) (Selection d)
foreign import transitionFn      :: ∀ d.   EffectFn1                 (Selection d) (Selection d)    -- changed from (s d) (t d)
foreign import transition2Fn     :: ∀ d.   EffectFn2 String          (Selection d) (Selection d)
foreign import delayFn           :: ∀ d.   EffectFn2 Time            (Selection d) (Selection d)
foreign import removeFn          :: ∀ d.   EffectFn1                 (Selection d) (Selection d)
foreign import selectAllFn       :: ∀ d.   EffectFn2 String          (Selection d) (Selection d)
foreign import selectFn          :: ∀ d.   EffectFn2 String          (Selection d) (Selection d)
foreign import selectionFn       :: ∀ d.   EffectFn1                 (Selection d) (Selection d) -- changed from (t d) (s d)
foreign import sizeFn            :: ∀ d.   EffectFn1                 (Selection d) Int
foreign import textFn            :: ∀ d v. EffectFn2 v               (Selection d) (Selection d)

-- need to define types to clean these sigs up   TODO
foreign import durationIFn       :: ∀ d.   EffectFn2    (Fn2 d Number Time)                    (Selection d) (Selection d)
foreign import delayIFn          :: ∀ d.   EffectFn2    (Fn2 d Number Time)                    (Selection d) (Selection d)
foreign import attrIFn           :: ∀ d v. EffectFn3    String (EffectFn3 d Index D3Element v) (Selection d) (Selection d)
foreign import styleIFn          :: ∀ d v. EffectFn3    String (EffectFn3 d Index D3Element v) (Selection d) (Selection d)

foreign import styleTweenFn      :: ∀ d v. EffectFn3    String (EffectFn3 d Index D3Element v) (Selection d) (Selection d)
foreign import textFnFn          :: ∀ d v v2. EffectFn2        (EffectFn2 v Index v2) (Selection d) (Selection d)

-- NB when using a saved transition you're going to be using it with different types of selections to this function will
-- morph it from type x to type d, the type of the selection to which it is being applied
foreign import savedTransitionFn :: ∀ d x. EffectFn2 (Selection x)  (Selection d)  (Selection d)    -- changed from (s d) (t d)

type D3DelayFn d = d -> Index -> Time

data TimeSpec d = MilliSec Time
                | DelayFn (D3DelayFn d)

data AttrInterpolator d v =
      Target v  -- straightforward target final value to tween to using built-in interpolators
    | TweenTarget (D3TweenTarget v d) -- function which is called a single time to get a target final value
    | TweenFn     (D3TweenFn     v d) -- function which is called once to generate a function which is then
                                      -- called every tween frame to generate a value

data D3Transition = TransitionName String
                  | UnnamedTransition

instance showD3Transition :: Show D3Transition where
  show (TransitionName s) = "Named transition: " <> show s
  show UnnamedTransition  = "Unnamed transition"

d3Transition :: ∀ d. D3Transition -> Effect (Selection d) -- in fact, d is unknown for transitions, they're really Type -> Type
d3Transition (TransitionName name)    = runEffectFn1 d3TransitionFn name
d3Transition UnnamedTransition        = runEffectFn1 d3TransitionFn "noname" -- d3TransitionFn2

addTransition :: ∀ d.  String -> Selection d  -> Effect (Selection d)
addTransition               = runEffectFn2 transition2Fn

makeSelection :: ∀ d.                             Selection d  -> Effect (Selection d)   -- changed from (t d) Eff (s d)
makeSelection              = runEffectFn1 selectionFn

makeTransition :: ∀ d.                             Selection d  -> Effect (Selection d)  -- changed from (s d) Eff (t d)
makeTransition              = runEffectFn1 transitionFn

namedTransition :: ∀ d. String                  -> Selection d  -> Effect (Selection d)  -- changed from (s d) Eff (t d)
namedTransition name        = runEffectFn2 namedTransitionFn name

savedTransition :: ∀ d x. (Selection x)        -> Selection d  -> Effect (Selection d)  -- changed from (s d) Eff (t d)
savedTransition name        = runEffectFn2 savedTransitionFn name

duration :: ∀ d. TimeSpec d                    -> Selection d -> Effect (Selection d)
duration (MilliSec t)       = runEffectFn2 durationFn t
duration (DelayFn f)        = runEffectFn2 durationIFn (mkFn2 f)

delay :: ∀ d. TimeSpec d                      -> Selection d -> Effect (Selection d)
delay (MilliSec t)          = runEffectFn2 delayFn t
delay (DelayFn f)           = runEffectFn2 delayIFn (mkFn2 f)

tEmpty :: ∀ d.                                     Selection d -> Effect Boolean
tEmpty                      = runEffectFn1 emptyFn

tFilter  :: ∀ d.  Filter d                      -> Selection d -> Effect (Selection d)
tFilter (Selector s)       = runEffectFn2 filterFn s
tFilter (Predicate p)      = runEffectFn2 filterFnP p

tNode :: ∀ d.                                      Selection d -> Effect (Maybe D3Element)
tNode t                     = toMaybe <$> runEffectFn1 nodeFn t

tNodes :: ∀ d.                                     Selection d -> Effect (Array D3Element)
tNodes                      = runEffectFn1 nodesFn

tMerge :: ∀ d.    Selection d                  -> Selection d -> Effect (Selection d)
tMerge                      = runEffectFn2 mergeFn

tRemove :: ∀ d.                                    Selection d -> Effect (Selection d) -- maybe this should be Void? TODO
tRemove                     = runEffectFn1 removeFn

tSize :: ∀ d.                                      Selection d -> Effect Int
tSize                       = runEffectFn1 sizeFn

tSelectAll :: ∀ d. String                        -> Selection d -> Effect (Selection d)
tSelectAll selector           = runEffectFn2 selectAllFn selector

tSelect  :: ∀ d.  String                         -> Selection d -> Effect (Selection d)
tSelect selector              = runEffectFn2 selectFn selector

-- replace this with same scheme as for Attributes TODO
-- tText  :: ∀ d v.  PolyValue d v                  -> Selection d -> Effect (Selection d)
-- tText       (Value value)     = runEffectFn2 textFn value
-- tText       (SetByIndex f)    = runEffectFn2 textFnFn (mkEffectFn2 f)


tAttr :: ∀ d v. String -> AttrInterpolator d v  -> Selection d -> Effect (Selection d)
tAttr name (Target v)       = runEffectFn3 attrFn       name v
tAttr name (TweenTarget f)  = runEffectFn3 attrIFn      name (mkEffectFn3 f)
tAttr name (TweenFn f)      = runEffectFn3 styleTweenFn name (mkEffectFn3 f)

tStyle :: ∀ d v. String -> AttrInterpolator d v -> Selection d -> Effect (Selection d)
tStyle name (Target v)      = runEffectFn3 styleFn      name v
tStyle name (TweenTarget f) = runEffectFn3 styleIFn     name (mkEffectFn3 f)
tStyle name (TweenFn f)     = runEffectFn3 styleTweenFn name (mkEffectFn3 f)
