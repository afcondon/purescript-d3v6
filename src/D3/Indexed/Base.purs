module D3.Indexed.Base where

import Prelude

import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad, iapply, imap)
import Control.Semigroupoid ((<<<))
import Data.List (List(..), (:))
import Data.Newtype (class Newtype)

foreign import data Element :: Type
foreign import data D3SelectionHandle :: Type

data Ready
data NoSelection
data OpenSelection
data JoinedSelection
data Transition

newtype Ingredient = Ingredient String
derive instance newtypeIngredient :: Newtype Ingredient _
type BurgerSpec = List Ingredient

newtype IxSelection i o spec = IxSelection spec

-- the function that will get your result out of the IndexedMonad
runIxSelection :: forall prev next model. IxSelection prev next model -> model
runIxSelection (IxSelection model) = model

-- instance ixBindIxSelection :: IxBind IxSelection where
--   ibind (IxSelection model) f = IxSelection <<< runIxSelection $ f model

-- instance ixApplicativeIxSelection :: IxApplicative IxSelection where
--   ipure = IxSelection

-- instance ixApplyIxSelection :: IxApply IxSelection where
--   iapply = iapply

-- instance ixFunctorIxSelection :: IxFunctor IxSelection where
--   imap = imap

noSelection :: IxSelection Ready NoSelection Unit
noSelection = IxSelection mempty

-- openSelection :: IxSelection NoSelection OpenSelection D3SelectionHandle
-- openSelection = IxSelection 


-- addIngredient :: forall i o. String -> BurgerSpec -> IxSelection i o (BurgerSpec)
-- addIngredient x xs = IxSelection $ Ingredient x : xs

