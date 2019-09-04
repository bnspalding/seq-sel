module Rhyme.ApproxSpec
  ( spec
  ) where

import qualified Rhyme.Approx as Approx
import Sound.Sound
import Sound.Stress
import qualified Sound.Syl as Syl
import Test.Hspec

spec :: Spec
spec = do
  describe "rhyme" $ do
    it "measures the feature similarity between the rhymes of two syls" $
      Approx.rhyme demoSylTruck demoSylTorque `shouldSatisfy`
      (> (Approx.rhyme demoSylTruck demoSylShaft))
    it "is normalized between the numbers 0 and 1" $
      Approx.rhyme demoSylTruck demoSylTorque `shouldSatisfy`
      (\x -> x >= 0 && x <= 1)
    it "should equal 1 when a syl is compared to itself" $
      Approx.rhyme demoSylTruck demoSylTruck `shouldSatisfy` (== 1)
    it "should equal 0 when a syl is compared to an empty syl" $
      Approx.rhyme demoSylTruck demoSylEmpty `shouldSatisfy` (== 0)
  describe "assonance" $ do
    it "measures the feature similarity between the nuclei of two syls" $
      Approx.assonance demoSylTruck demoSylTorque `shouldSatisfy`
      (> (Approx.assonance demoSylTruck demoSylShaft))
    it "is normalized between the numbers 0 and 1" $
      Approx.assonance demoSylTruck demoSylTorque `shouldSatisfy`
      (\x -> x >= 0 && x <= 1)
    it "should equal 1 when a syl is compared to itself" $
      Approx.assonance demoSylTruck demoSylTruck `shouldSatisfy` (== 1)
    it "should equal 0 when a syl is compared to an empty syl" $
      Approx.assonance demoSylTruck demoSylEmpty `shouldSatisfy` (== 0)
  describe "alliteration" $ do
    it "measures the feature similarity between the onsets of two syls" $
      Approx.alliteration demoSylTruck demoSylTorque `shouldSatisfy`
      (> (Approx.alliteration demoSylTruck demoSylShaft))
    it "is normalized between the numbers 0 and 1" $
      Approx.alliteration demoSylTruck demoSylTorque `shouldSatisfy`
      (\x -> x >= 0 && x <= 1)
    it "should equal 1 when a syl is compared to itself" $
      Approx.alliteration demoSylTruck demoSylTruck `shouldSatisfy` (== 1)
    it "should equal 0 when a syl is compared to an empty syl" $
      Approx.alliteration demoSylTruck demoSylEmpty `shouldSatisfy` (== 0)

demoSylTruck :: Syl.Syl
demoSylTruck =
  Syl.Syl
    { Syl.onset = Sound <$> ["t", "ɹ"]
    , Syl.nucleus = Sound <$> ["ʌ"]
    , Syl.coda = Sound <$> ["k"]
    , Syl.stress = NullStress
    }

demoSylTorque :: Syl.Syl
demoSylTorque =
  Syl.Syl
    { Syl.onset = Sound <$> ["t"]
    , Syl.nucleus = Sound <$> ["ɔ"]
    , Syl.coda = Sound <$> ["ɹ", "k"]
    , Syl.stress = NullStress
    }

demoSylShaft :: Syl.Syl
demoSylShaft =
  Syl.Syl
    { Syl.onset = Sound <$> ["ʃ"]
    , Syl.nucleus = Sound <$> ["æ"]
    , Syl.coda = Sound <$> ["f", "t"]
    , Syl.stress = NullStress
    }

demoSylEmpty :: Syl.Syl
demoSylEmpty =
  Syl.Syl
    {Syl.onset = [], Syl.nucleus = [], Syl.coda = [], Syl.stress = NullStress}
