module D3.Foreign where

-- | these foreign types allow us to work with some very funky typing without 
-- | adding tonnes of syntactic noise or type complexity
-- | NativeSelection is an opaque type which we keep in order to feed it back to D3 
-- | when we look up nameSelection Selections, Transitions, Simulations, whatever
foreign import data NativeSelection :: Type

-- | The Datum models the (variable / polymorphic) type of the lambdas used in Attr
foreign import data Datum       :: Type

-- | The SubModel is that portion of the Model that we give to a particular Join
foreign import data SubModel    :: Type


-- TODO move to separate Scales module when more fleshed out
type Scale = Number -> String

foreign import d3SchemeCategory10JS :: Scale -- not modelling the scale / domain distinction yet
