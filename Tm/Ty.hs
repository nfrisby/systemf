module Tm.Ty where

import Data.Kind (Type)

import N
import Ty

-- | Type context as number of type variables in scope.
type TCtx = N

-- | An in-scope type variable.
data GT :: TCtx -> TVar -> Type where
  GTZ :: GT ('S n) 'Z
  GTS :: GT n m -> GT ('S n) ('S m)

-- | Type terms. The 'Ty' index is the promotion of the type itself;
-- the 'TTm' type also serves both as a singleton type for the 'Ty'
-- data kind and also as a proof that the type is well-formed in the
-- given type context.
data TTm :: TCtx -> Ty -> Type where
  TTmFun :: TTm tgam a -> TTm tgam b -> TTm tgam ('Fun a b)
  TTmForAll :: TTm ('S tgam) b -> TTm tgam ('ForAll b)
  TTmVar :: GT tgam n -> TTm tgam ('TVar n)
