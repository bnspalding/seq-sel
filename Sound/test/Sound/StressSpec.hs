module Sound.StressSpec
  ( spec
  ) where

import Sound.Stress
import Test.Hspec

spec :: Spec
spec =
  describe "Stress Levels" $ do
    it "ReducedStress is LowStress" $
      ReducedStress `shouldSatisfy`
      (\x -> isLowStress x && not (isHighStress x))
    it "Unstressed is LowStress" $
      Unstressed `shouldSatisfy` (\x -> isLowStress x && not (isHighStress x))
    it "SecondaryStress is HighStress" $
      SecondaryStress `shouldSatisfy`
      (\x -> isHighStress x && not (isLowStress x))
    it "Stressed is HighStress" $
      Stressed `shouldSatisfy` (\x -> isHighStress x && not (isLowStress x))
    it "NullStress is neither HighStress or LowStress" $
      NullStress `shouldSatisfy` (\x -> not (isHighStress x || isLowStress x))
