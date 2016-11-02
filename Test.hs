{-# Language ScopedTypeVariables #-}

module Test where

import N
import Ty
import Tm.Ty
import Tm
import Tm.Interpret

-- | @/\\a. \\(x :: a). x@
ex_id :: Tm tgam gam ('ForAll ('Fun ('TVar 'Z) ('TVar 'Z)))
ex_id = TLam $ Lam $ Var IZ

-- | @/\\y. 'ex_id' [y]@
ex_id2 :: Tm tgam gam ('ForAll ('Fun ('TVar 'Z) ('TVar 'Z)))
ex_id2 = TLam $ TApp ex_id (TTmVar GTZ)

-- | @'ex_id' [X] ('ex_id2' [X] A)@
--
-- where @X@ and @A :: X@ are free as the @0@-th variable.
ex_free :: Tm ('S tgam) ('TVar 'Z ': gam) ('TVar 'Z)
ex_free = TApp ex_id (TTmVar GTZ) `App` (TApp ex_id2 (TTmVar GTZ) `App` Var IZ)

interp_ex_free :: forall a. a -> a
interp_ex_free x = interpret rho ex_free
  where
  rho :: Env '[a] '[ 'TVar 'Z ]
  rho = ES EZ x

-----

infixr 0 :->
type a :-> b = 'Fun a b

type B = 'ForAll ('TVar 'Z :-> 'TVar 'Z :-> 'TVar 'Z)

tB :: TTm tgam B
tB = TTmForAll $ TTmFun (TTmVar GTZ) (TTmFun (TTmVar GTZ) (TTmVar GTZ))

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

tIfThenElse :: Tm tgam gam ('ForAll (B :-> 'TVar 'Z :-> 'TVar 'Z :-> 'TVar 'Z))
tIfThenElse = TLam $ Lam $ Lam $ Lam $ Var (IS (IS IZ)) `TApp` TTmVar GTZ `App` Var (IS IZ) `App` Var IZ

interp_tIfThenElse :: forall a. (forall tgam gam. Tm tgam gam B) -> a -> a -> a
interp_tIfThenElse b = interpret rho $
         tIfThenElse
  `TApp` TTmVar GTZ
   `App` b
  where
  rho :: Env '[a] '[]
  rho = EZ
