{-# Language UndecidableInstances #-}

module Ty.Subst where

import N
import Ty

-- | Substituate @image@ for @0@.
type Subst image ty = Subst_ 'Z image ty

-- | Replace @m@ with @image@, and decrement all variables @> m@.
type family Subst_ (m :: TVar) (image :: Ty) (ty :: Ty) :: Ty where
  Subst_ m image ('Fun a b) = 'Fun (Subst_ m image a) (Subst_ m image b)
  Subst_ m image ('ForAll b) = 'ForAll (Subst_ ('S m) image b)   -- undecidable
  Subst_ m image ('TVar n) = Subst_Var m image n   -- undecidable

-- | Replace @m@ with @image@, and decrement any variable @> m@.
type Subst_Var m image n = Subst_Var_ 'Z m image n

-- | Replace @o + m@ with @image@, and decrement any variable @> o + m@.
type family Subst_Var_ (o :: N) (m :: N) (image :: Ty) (n :: TVar) :: Ty where
  Subst_Var_ _ 'Z image 'Z = image
  Subst_Var_ o 'Z _ ('S n) = 'TVar (Add o n)
  Subst_Var_ o ('S m) image 'Z = 'TVar o
  Subst_Var_ o ('S m) image ('S n) = Subst_Var_ ('S o) m image n
