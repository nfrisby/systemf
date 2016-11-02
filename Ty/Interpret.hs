{-# Language ScopedTypeVariables #-}

module Ty.Interpret where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))

import N
import Ty

-- | A type environment maps type variables to type interpretations.
type TEnv = [Type]

-- | Interpret a type inhabiting 'Ty' as a type inhabiting 'Type'.
type family Interpret (trho :: TEnv) (ty :: Ty) :: Type where
  Interpret trho ('Fun a b) = Interpret trho a -> Interpret trho b
  Interpret trho ('TVar n) = Lookup n trho
  Interpret trho ('ForAll b) = FA trho b

type family Lookup (n :: TVar) (trho :: TEnv) :: Type where
  Lookup 'Z (a ': _) = a
  Lookup ('S n) (_ ': trho) = Lookup n trho

-- | This type wraps the @forall@ so that it can occur on the right-hand side of
-- the 'Interpret' type family's match for ''ForAll'.
newtype FA (trho :: TEnv) (b :: Ty) =
  FA (forall a. Proxy a -> Interpret (a ': trho) b)

-- | Instantiate an 'FA' at the given 'Ty' type.
unFA :: forall p1 trho b ty. FA trho b -> p1 ty -> Interpret (Interpret trho ty ': trho) b
unFA (FA f) _ = f (Proxy :: Proxy (Interpret trho ty))
