{-# Language GADTs #-}
{-# Language     Rank2Types #-}
{-# Language     TypeFamilies #-}
{-# Language     TypeInType #-}
{-# Language     TypeOperators #-}
{-# Language ScopedTypeVariables #-}

module Test where

import Data.Kind (Type)

import Idx
import Ty
import Ty.Interpret
import Tm.Ty
import Tm
import Tm.Interpret

-- | @/\\a. \\(x :: a). x@
ex_id :: Tm tgam gam ('ForAll ('Fun ('TVar 'IZ) ('TVar 'IZ)))
ex_id = TLam $ Lam $ Var IZ

-- | @/\\y. 'ex_id' [y]@
ex_id2 :: Tm tgam gam ('ForAll ('Fun ('TVar 'IZ) ('TVar 'IZ)))
ex_id2 = TLam $ TApp ex_id (TTmVar SIZ)

-- | @'ex_id' [X] ('ex_id2' [X] A)@
--
-- where @X@ and @A :: X@ are free as the @0@-th variable.
ex_free :: Tm ('KStar ': tgam) ('TVar 'IZ ': gam) ('TVar 'IZ)
ex_free = TApp ex_id (TTmVar SIZ) `App` (TApp ex_id2 (TTmVar SIZ) `App` Var IZ)

interp_ex_free :: forall a. a -> a
interp_ex_free x = interpret rho ex_free
  where
  rho :: Env ('TES 'TEZ a) '[ 'TVar 'IZ ]
  rho = ES EZ x

interp_ex_free1 :: forall f (a :: Type). f a -> f a
interp_ex_free1 x = interpret rho (Var IZ)
  where
  rho :: Env ('TES ('TES 'TEZ f) a :: TEnv '[ 'KStar , 'KFun 'KStar 'KStar ]) '[ 'TyApp ('TVar ('IS 'IZ)) ('TVar 'IZ) ]
  rho = ES EZ x

-----

infixr 0 :->
type a :-> b = 'Fun a b

type B = 'ForAll ('TVar 'IZ :-> 'TVar 'IZ :-> 'TVar 'IZ)

tB :: TTm tgam 'KStar B
tB = TTmForAll $ TTmFun (TTmVar SIZ) (TTmFun (TTmVar SIZ) (TTmVar SIZ))

tTrue :: Tm tgam gam B
tTrue = TLam $ Lam $ Lam $ Var (IS IZ)

tFalse :: Tm tgam gam B
tFalse = TLam $ Lam $ Lam $ Var IZ

tAnd :: Tm tgam gam (B :-> B :-> B)
tAnd = Lam $ Lam $ Var (IS IZ) `TApp` tB `App` Var IZ `App` tFalse

tOr :: Tm tgam gam (B :-> B :-> B)
tOr = Lam $ Lam $ Var (IS IZ) `TApp` tB `App` tTrue `App` Var IZ

tNot :: Tm tgam gam (B :-> B)
tNot = Lam $ Var IZ `TApp` tB `App` tTrue `App` tFalse

tIfThenElse :: Tm tgam gam ('ForAll (B :-> 'TVar 'IZ :-> 'TVar 'IZ :-> 'TVar 'IZ))
tIfThenElse = TLam $ Lam $ Lam $ Lam $ Var (IS (IS IZ)) `TApp` TTmVar SIZ `App` Var (IS IZ) `App` Var IZ

interp_tIfThenElse :: forall a. (forall tgam gam. Tm tgam gam B) -> a -> a -> a
interp_tIfThenElse b = interpret rho $
         tIfThenElse
  `TApp` TTmVar SIZ
   `App` b
  where
  rho :: Env ('TES 'TEZ a :: TEnv '[ 'KStar ]) '[]
  rho = EZ
