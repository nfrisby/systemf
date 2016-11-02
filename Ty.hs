module Ty where

import N

-- | Type variable as a de Bruijn index.
type TVar = N

-- | Types.
data Ty = Fun Ty Ty | ForAll Ty | TVar TVar
