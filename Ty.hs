module Ty where

import Data.Kind (Type)

import Idx

data Ki = KStar | KFun Ki Ki

-- | Type context as kinds of type variables in scope.
type TCtx = [Ki]

-- | Types.
data Ty :: TCtx -> Ki -> Type where
  Fun :: Ty tgam 'KStar -> Ty tgam 'KStar -> Ty tgam 'KStar
  ForAll :: Ty (ka ': tgam) 'KStar -> Ty tgam 'KStar
  TyApp :: Ty tgam ('KFun a b) -> Ty tgam a -> Ty tgam b
  TVar :: Idx tgam k -> Ty tgam k
