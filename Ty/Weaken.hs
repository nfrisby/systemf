{-# Language UndecidableInstances #-}

module Ty.Weaken (Weaken) where

import N
import Idx
import Ty

-- | Increment all variables.
type Weaken ty = Weaken_ 'Z ty

-- | Increment all variables @>= m@.
type family Weaken_ (m :: N) (ty :: Ty tgam kb) :: Ty (ka ': tgam) kb where
  Weaken_ m ('Fun a b) = 'Fun (Weaken_ m a) (Weaken_ m b)   -- undecidable
  Weaken_ m ('ForAll b) = 'ForAll (Weaken_ ('S m) b)   -- undecidable
  Weaken_ m ('TyApp a b) = 'TyApp (Weaken_ m a) (Weaken_ m b)   -- undecidable
  Weaken_ m ('TVar n) = 'TVar (Weaken_Var m n)

-- | Increment any variable @>= m@.
type family Weaken_Var (m :: N) (n :: Idx tgam kb) :: Idx ((ka :: Ki) ': tgam) kb where
  Weaken_Var 'Z n = 'IS n
  Weaken_Var ('S m) 'IZ = 'IZ
  Weaken_Var ('S m) ('IS n) = 'IS (Weaken_Var m n)
