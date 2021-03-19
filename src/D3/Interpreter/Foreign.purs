module D3.Interpreter.Foreign where

import D3.Base
import Prelude

import Data.Nullable (Nullable)
import Unsafe.Coerce (unsafeCoerce)

-- | right upfront we're just going to hand wave over the types given to D3
-- | design decision here to prefer (aliased) `unsafeCoerce` to complex wrappers
-- | D3 WILL modify any data you give it, it will also require types that CANNOT be
-- | expressed in PureScript AND also transparently shared with JavaScript 
-- | (ie Variants, Exists, Nullable etc)
-- | IMPORTANT - coercions should not be visible outside this module
foreign import data NativeJS       ::           Type

-- | foreign types associated with D3 Selection 
-- all this FFI stuff can be made Effect Unit later 
foreign import runSimpleAttrJS     ::           NativeSelection -> String -> NativeJS -> Unit
foreign import runDatumAttrJS      :: forall f. NativeSelection -> String -> f -> Unit
foreign import runDatumTextJS      :: forall f. NativeSelection           -> f -> Unit
foreign import runDatumIndexAttrJS :: forall f. NativeSelection -> String -> f -> Unit
foreign import d3SelectAllJS       ::           String -> NativeSelection
foreign import d3AppendElementJS   ::           NativeSelection -> String -> NativeSelection
-- foreign import d3EnterElementJS :: NativeSelection -> String -> NativeSelection
foreign import d3JoinJS :: forall d. 
     NativeSelection
  -> String
  -> d
  -> { enter  :: NativeSelection -> NativeSelection,
       update :: NativeSelection -> NativeSelection,
       exit   :: NativeSelection -> NativeSelection }
  -> NativeSelection

foreign import d3EnterWithIndexJS  :: forall d. NativeSelection -> d -> (d -> NativeJS) -> NativeSelection
foreign import nullSelectionJS     ::           NativeSelection

-- | foreign types associated with Force Layout Simulation
foreign import initSimulationJS :: SimulationConfig -> NativeSelection
-- TODO tick functions should be nameSelection too, so this should be param'd with a tick NativeSelection too!
foreign import addAttrFnToTickJS :: NativeSelection -> Attr -> Unit
foreign import attachTickFnToSimulationJS :: NativeSelection -> Unit
foreign import attachDefaultDragBehaviorJS :: NativeSelection -> NativeSelection -> Unit
foreign import putNodesInSimulationJS :: NativeSelection -> Array NativeJS -> Array NativeJS
foreign import putLinksInSimulationJS :: NativeSelection -> Array NativeJS -> Array NativeJS
foreign import startSimulationJS      :: NativeSelection -> Unit
foreign import stopSimulationJS       :: NativeSelection -> Unit
foreign import setAlphaTargetJS       :: NativeSelection -> Number -> Unit
foreign import forceManyJS            :: NativeSelection -> String -> Unit
foreign import forceCenterJS          :: NativeSelection -> String -> Number -> Number -> Unit
foreign import forceCollideJS         :: NativeSelection -> String -> Number -> Unit
foreign import forceXJS               :: NativeSelection -> String -> Number -> Unit
foreign import forceYJS               :: NativeSelection -> String -> Number -> Unit
foreign import forceRadialJS          :: NativeSelection -> String -> Number -> Number -> Unit

stringToNativeJS :: String -> NativeJS
stringToNativeJS = unsafeCoerce

numberToNativeJS :: Number -> NativeJS
numberToNativeJS = unsafeCoerce

arrayNumberToNativeJS :: Array Number -> NativeJS
arrayNumberToNativeJS = unsafeCoerce
