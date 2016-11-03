module Tm.Ty where

import Data.Kind (Type)

import Idx
import Ty

-- | Type terms. The 'Ty' index is the promotion of the type itself;
-- the 'TTm' type also serves both as a singleton type for the 'Ty'
-- data kind and also as a proof that the type is well-formed in the
-- given type context.
data TTm (tgam :: TCtx) (k :: Ki) (ty :: Ty tgam k) :: Type where
  TTmFun :: TTm tgam 'KStar a -> TTm tgam 'KStar b -> TTm tgam 'KStar ('Fun a b)
  TTmForAll :: TTm (ka ': tgam) 'KStar b -> TTm tgam 'KStar ('ForAll b)
  TTmVar :: SIdx tgam k i -> TTm tgam k ('TVar i)
