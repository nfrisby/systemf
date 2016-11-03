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
    p1 k
 -> p2 a
 -> p3 (trho :: TEnv tgam) (ty ': x1)
 -> Coercion
      (Interpret trho ty)
      (Interpret ('TES trho a :: TEnv (k ': tgam)) (Weaken ty))
lemmaWeaken _ _ _ = unsafeCoerce (Coercion :: Coercion () ())

-- | Lemma relating interpretation and substitution for types.
--
-- Proof by assertion.
--
-- It would be sound to upgrade 'Coercion' to 'Data.Type.Equality.:~:'
-- if it weren't for the 'FA' wrapper.
lemmaSubst ::
    p1 (trho :: TEnv tgam) gam
 -> p2 ('ForAll fun)
 -> p3 ty
 -> Coercion
      (Interpret ('TES trho (Interpret trho ty)) fun)
      (Interpret trho (Subst ty fun))
lemmaSubst _ _ _ = unsafeCoerce (Coercion :: Coercion () ())
