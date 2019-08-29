module Stress where

data Stress
  = ReducedStress
  | Unstressed
  | SecondaryStress
  | Stressed
  deriving (Ord, Eq, Show)

isLowStress :: Stress -> Bool
isLowStress s = s <= Unstressed

isHighStress :: Stress -> Bool
isHighStress s = s >= SecondaryStress
