module Tm where

import Data.Kind (Type)

import N
import Ty
import Ty.Weaken
import Ty.Subst
import Tm.Ty

-- | Term context as types of term variables in scope.
type Ctx = [Ty]

-- | An in-scope term variable.
data Idx :: Ctx -> Ty -> Type where
  IZ :: Idx (a ': as) a
  IS :: Idx as a -> Idx (b ': as) a

-- | Weaken the types in the context.
type family WeakenCtx (gam :: Ctx) :: Ctx where
  WeakenCtx '[] = '[]
  WeakenCtx (ty ': tys) = Weaken ty ': WeakenCtx tys

-- | Terms. The 'Ty' index is the type of the term.
data Tm :: TCtx -> Ctx -> Ty -> Type where
  Var :: Idx gam a -> Tm tgam gam a
  Lam :: Tm tgam (a ': gam) b -> Tm tgam gam ('Fun a b)
  App :: Tm tgam gam ('Fun a b) -> Tm tgam gam a -> Tm tgam gam b
  TLam :: Tm ('S tgam) (WeakenCtx gam) b -> Tm tgam gam ('ForAll b)
  TApp :: Tm tgam gam ('ForAll b) -> TTm tgam a -> Tm tgam gam (Subst a b)
