module Tm where

import Data.Kind (Type)

import Idx
import Ty
import Ty.Weaken
import Ty.Subst
import Tm.Ty

-- | Term context as types of term variables in scope.
type Ctx tgam = [Ty tgam 'KStar]

-- | Weaken the types in the context.
type family WeakenCtx (gam :: Ctx tgam) :: Ctx (ka ': tgam) where
  WeakenCtx '[] = '[]
  WeakenCtx (ty ': tys) = Weaken ty ': WeakenCtx tys

-- | Terms. The 'Ty' index is the type of the term.
data Tm (tgam :: TCtx) (gam :: Ctx tgam) (ty :: Ty tgam 'KStar) :: Type where
  Var :: Idx gam a -> Tm tgam gam a
  Lam :: Tm tgam (a ': gam) b -> Tm tgam gam ('Fun a b)
  App :: Tm tgam gam ('Fun a b) -> Tm tgam gam a -> Tm tgam gam b
  TLam :: Tm (k ': tgam) (WeakenCtx gam) b -> Tm tgam gam ('ForAll b)
  TApp :: Tm tgam gam ('ForAll b) -> TTm tgam k a -> Tm tgam gam (Subst a b)
