{-# Language UndecidableInstances #-}

module Ty.Weaken where

import N
import Ty

-- | Increment all variables.
type Weaken ty = Weaken_ 'Z ty

-- | Increment all variables @>= m@.
type family Weaken_ (m :: N) (ty :: Ty) :: Ty where
  Weaken_ m ('Fun a b) = 'Fun (Weaken_ m a) (Weaken_ m b)   -- undecidable
  Weaken_ m ('ForAll b) = 'ForAll (Weaken_ ('S m) b)   -- undecidable
  Weaken_ m ('TVar n) = 'TVar (Weaken_Var m n)

-- | Increment any variable @>= m@.
type family Weaken_Var (m :: N) (n :: TVar) :: TVar where
  Weaken_Var 'Z n = 'S n
  Weaken_Var ('S m) 'Z = 'Z
  Weaken_Var ('S m) ('S n) = 'S (Weaken_Var m n)
