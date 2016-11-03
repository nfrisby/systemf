{-# Language UndecidableInstances #-}

module Ty.Subst (Subst) where

import Data.Kind (Type)

import Idx
import Ty
import Ty.Weaken

data C :: Ki -> TCtx -> TCtx -> Type where
  CZ :: Ty tgam ka -> C ka tgam (ka ': tgam)
  CS :: C ka tgam tgam' -> C ka (k ': tgam) (k ': tgam')

-- | Substituate @image@ for @0@.
type Subst image ty = Subst_ ('CZ image) ty

-- | Replace @m@ with @image@, and decrement all variables @> m@.
type family Subst_ (m :: C ka tgam tgam') (ty :: Ty tgam' kb) :: Ty tgam kb where
  Subst_ m ('Fun a b) = 'Fun (Subst_ m a) (Subst_ m b)
  Subst_ m ('ForAll b) = 'ForAll (Subst_ ('CS m) b)   -- undecidable
  Subst_ m ('TyApp a b) = 'TyApp (Subst_ m a) (Subst_ m b)
  Subst_ m ('TVar n) = Subst_Var_ m n   -- undecidable

type family Subst_Var_ (m :: C ka tgam tgam') (n :: Idx tgam' kb) :: Ty tgam kb where
  Subst_Var_ ('CZ image) 'IZ = image
  Subst_Var_ ('CS m) 'IZ = 'TVar 'IZ
  Subst_Var_ ('CS m) ('IS n) = Weaken (Subst_Var_ m n)
