module Tm.Interpret where

import Data.Kind (Type)
import Data.Type.Coercion (coerceWith)

import Ty.Interpret
import Ty.Lemmas
import Tm

-- | A term environment maps term variables to term
-- interpretations. It's indexed by the type environment and term
-- context.
data Env :: TEnv -> Ctx -> Type where
  EZ :: Env trho '[]
  ES :: Env trho gam -> Interpret trho ty -> Env trho (ty ': gam)

lookupEnv :: Idx gam ty -> Env trho gam -> Interpret trho ty
lookupEnv IZ (ES _ a) = a
lookupEnv (IS i) (ES rho _) = lookupEnv i rho

weakenEnv :: prx a -> Env trho gam -> Env (a ': trho) (WeakenCtx gam)
weakenEnv prx rho0 = case rho0 of
  EZ -> EZ
  ES rho a ->
    ES (weakenEnv prx rho)
       (coerceWith (lemmaWeaken prx rho0) a)

-- | Interpret a term in the given type and term environments.
interpret :: Env trho gam -> Tm tgam gam ty -> Interpret trho ty
interpret rho (Var i) = lookupEnv i rho
interpret rho (Lam b) = \a -> interpret (ES rho a) b
interpret rho (App f a) = (interpret rho f) (interpret rho a)
interpret rho (TApp f tt) =
    coerceWith (lemmaSubst rho f tt)
  $ interpret rho f `unFA` tt
interpret rho (TLam b) = FA $ \prx -> interpret (weakenEnv prx rho) b
