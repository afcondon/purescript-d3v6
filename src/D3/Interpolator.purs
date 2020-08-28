module D3.Interpolator
  ( Time
  , Index
  , D3TweenFnUncurried
  , D3TweenTargetUncurried
  , D3TweenFn
  , D3TweenTarget
  , D3EffTweenFn
  , D3EffTweenTarget
  -- , mkTweenTargetEffectFn
  -- , mkTweenFunctionEffectFn
  ) where

import Effect
import D3.Base (D3Element)
type Time  = Number
type Index = Number

-- foreign import data TweenFn :: * -> *
foreign import data D3EffTweenTarget :: Type -> Type -> Type -> Type -> Type
type D3TweenTargetUncurried v d = D3EffTweenTarget d Index D3Element                     v
type D3TweenTarget          v d = d -> Index -> D3Element -> Effect v

foreign import data D3EffTweenFn :: Type -> Type -> Type -> Type -> Type
type D3TweenFnUncurried v d = D3EffTweenFn d Index D3Element (Time -> v)
type D3TweenFn          v d = d -> Index -> D3Element -> Effect (Time -> v)
