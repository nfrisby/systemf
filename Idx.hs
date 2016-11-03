module Idx where

import Data.Kind (Type)

-- | An in-scope variable.
data Idx :: [k] -> k -> Type where
  IZ :: Idx (a ': as) a
  IS :: Idx as a -> Idx (b ': as) a

data SIdx (as :: [k]) (a :: k) (i :: Idx as a) :: Type where
  SIZ :: SIdx (a ': as) a 'IZ
  SIS :: SIdx as a i -> SIdx (b ': as) a ('IS i)
