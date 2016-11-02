module Ty.Lemmas where

import Data.Type.Coercion (Coercion(..))
import Unsafe.Coerce (unsafeCoerce)

import Ty
import Ty.Weaken
import Ty.Subst
import Ty.Interpret

-- | Lemma relating interpretation and weakening for types.
--
-- Proof by assertion.
--
-- It would be sound to upgrade 'Coercion' to 'Data.Type.Equality.:~:'
-- if it weren't for the 'FA' wrapper.
lemmaWeaken ::
    p1 a
 -> p2 trho (ty ': x1)
 -> Coercion
      (Interpret trho ty)
      (Interpret (a ': trho) (Weaken ty))
lemmaWeaken _ _ = unsafeCoerce (Coercion :: Coercion () ())

-- | Lemma relating interpretation and substitution for types.
--
-- Proof by assertion.
--
-- It would be sound to upgrade 'Coercion' to 'Data.Type.Equality.:~:'
-- if it weren't for the 'FA' wrapper.
lemmaSubst ::
    p1 trho (x1 :: k1)
 -> p2 (x2 :: k2) (x3 :: k3) ('ForAll fun)
 -> p3 (x4 :: k4) ty
 -> Coercion
      (Interpret (Interpret trho ty ': trho) fun)
      (Interpret trho (Subst ty fun))
lemmaSubst _ _ _ = unsafeCoerce (Coercion :: Coercion () ())
