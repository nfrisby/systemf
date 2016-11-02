module N where

data N = Z | S N

type family Add (n :: N) (m :: N) :: N where
  Add 'Z m = m
  Add ('S n) m = 'S (Add n m)
