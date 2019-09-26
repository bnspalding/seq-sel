module Sound.Stress where

data Stress
  = NullStress
  | ReducedStress
  | Unstressed
  | SecondaryStress
  | Stressed
  deriving (Ord, Eq, Show)

isLowStress :: Stress -> Bool
isLowStress NullStress = False
isLowStress s = s <= Unstressed

isHighStress :: Stress -> Bool
isHighStress s = s >= SecondaryStress

maybeStress :: Stress -> Maybe Stress
maybeStress NullStress = Nothing
maybeStress s = Just s

stressSymbolIPA :: String
stressSymbolIPA = "ˈ" -- 02C8

secondaryStressSymbolIPA :: String
secondaryStressSymbolIPA = "ˌ" --02CC
