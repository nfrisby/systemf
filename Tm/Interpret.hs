{-# Language UndecidableInstances #-}

module Tm.Interpret where

import Data.Kind (Type)
import Data.Type.Coercion (coerceWith)

import Idx
import Ty
import Ty.Interpret
import Ty.Lemmas
import Tm

-- | This works around a GHC bug; if I inline it or use a type synonym,
-- I get a panic @kindPrimRep.go ''KStar'@.
type family Interpret' (trho :: TEnv tgam) (ty :: Ty tgam 'KStar) :: Type where
  Interpret' trho ty = Interpret trho ty

-- | A term environment maps term variables to term
-- interpretations. It's indexed by the type environment and term
-- context.
data Env :: TEnv tgam -> Ctx tgam -> Type where
  EZ :: Env trho '[]
  ES :: Env trho gam -> Interpret' trho ty -> Env trho (ty ': gam)

lookupEnv :: Idx gam ty -> Env trho gam -> Interpret' trho ty
lookupEnv IZ (ES _ a) = a
lookupEnv (IS i) (ES rho _) = lookupEnv i rho

weakenEnv :: pk k -> pa a -> Env (trho :: TEnv tgam) gam -> Env ('TES trho a :: TEnv (k ': tgam)) (WeakenCtx gam)
weakenEnv pk pa rho0 = case rho0 of
  EZ -> EZ
  ES rho a ->
    ES (weakenEnv pk pa rho)
       (coerceWith (lemmaWeaken pk pa rho0) a)

-- | Interpret a term in the given type and term environments.
interpret :: Env trho gam -> Tm tgam gam ty -> Interpret' trho ty
interpret rho (Var i) = lookupEnv i rho
interpret rho (Lam b) = \a -> interpret (ES rho a) b
interpret rho (App f a) = (interpret rho f) (interpret rho a)
interpret rho (TApp f tt) =
    coerceWith (lemmaSubst rho f tt)
  $ interpret rho f `unFA` tt
interpret rho (TLam b) = FA $ \pk pa -> interpret (weakenEnv pk pa rho) b
