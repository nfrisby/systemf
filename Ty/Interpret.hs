{-# Language ScopedTypeVariables #-}

module Ty.Interpret where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))

import Idx
import Ty

type family InterpretK (k :: Ki) :: Type where
  InterpretK 'KStar = Type
  InterpretK ('KFun a b) = InterpretK a -> InterpretK b

-- | A type environment maps type variables to type interpretations.
data TEnv :: [Ki] -> Type where
  TEZ :: TEnv '[]
  TES :: TEnv ks -> InterpretK k -> TEnv (k ': ks)

-- | Interpret a type inhabiting 'Ty' as a type inhabiting 'Type'.
type family Interpret (trho :: TEnv tgam) (ty :: Ty tgam k) :: InterpretK k where
  Interpret trho ('Fun a b) = Interpret trho a -> Interpret trho b
  Interpret trho ('TyApp a b) = (Interpret trho a) (Interpret trho b)
  Interpret trho ('TVar n) = Lookup n trho
  Interpret trho ('ForAll b) = FA trho b

type family Lookup (n :: Idx tgam k) (trho :: TEnv tgam) :: InterpretK k where
  Lookup 'IZ ('TES _ a) = a
  Lookup ('IS n) ('TES trho _) = Lookup n trho

-- | This type wraps the @forall@ so that it can occur on the right-hand side of
-- the 'Interpret' type family's match for ''ForAll'.
newtype FA (trho :: TEnv tgam) (b :: Ty (ka ': tgam) 'KStar) =
  FA (forall (a :: InterpretK ka). Proxy ka -> Proxy a -> Interpret ('TES trho a) b)

-- | Instantiate an 'FA' at the given 'Ty' type.
unFA ::
  forall ka tgam p1 trho b ty.
     FA (trho :: TEnv tgam) b
  -> p1 (ty :: Ty tgam ka)
  -> Interpret ('TES trho (Interpret trho ty) :: TEnv (ka ': tgam)) b
unFA (FA f) _ = f (Proxy :: Proxy ka) (Proxy :: Proxy (Interpret trho ty))
