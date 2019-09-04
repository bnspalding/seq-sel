module Rhyme.StrictSpec
  ( spec
  ) where

import qualified Rhyme.Strict as Strict
import Sound.Sound
import Sound.Stress
import qualified Sound.Syl as Syl
import Test.Hspec

spec :: Spec
spec = do
  describe "rhyme" $ do
    it "checks equality between the rhymes of two syls" $
      Strict.rhyme demoSylTik demoSylTok `shouldBe` False
    it "is true only when the sounds are the same" $
      Strict.rhyme demoSylTik demoSylTik `shouldBe` True
    it "does not match any syl to an empty syl" $
      Strict.rhyme demoSylTik demoSylEmpty `shouldBe` False
    it "matches an empty syl to an empty syl" $
      Strict.rhyme demoSylEmpty demoSylEmpty `shouldBe` True
  describe "assonance" $ do
    it "checks equality betwee the nuclei of two syls" $
      Strict.assonance demoSylTik demoSylTok `shouldBe` False
    it "is true only when the sounds are the same" $
      Strict.assonance demoSylTik demoSylShift `shouldBe` True
  describe "alliteration" $ do
    it "checks equality between the onsets of two syls" $
      Strict.alliteration demoSylTik demoSylShift `shouldBe` False
    it "is true only when the sounds are the same" $
      Strict.alliteration demoSylTik demoSylTok `shouldBe` True

demoSylTik :: Syl.Syl
demoSylTik =
  Syl.Syl
    { Syl.onset = Sound <$> ["t"]
    , Syl.nucleus = Sound <$> ["ɪ"]
    , Syl.coda = Sound <$> ["k"]
    , Syl.stress = NullStress
    }

demoSylTok :: Syl.Syl
demoSylTok =
  Syl.Syl
    { Syl.onset = Sound <$> ["t"]
    , Syl.nucleus = Sound <$> ["ɔ"]
    , Syl.coda = Sound <$> ["k"]
    , Syl.stress = NullStress
    }

demoSylShift :: Syl.Syl
demoSylShift =
  Syl.Syl
    { Syl.onset = Sound <$> ["ʃ"]
    , Syl.nucleus = Sound <$> ["ɪ"]
    , Syl.coda = Sound <$> ["f", "t"]
    , Syl.stress = NullStress
    }

demoSylEmpty :: Syl.Syl
demoSylEmpty =
  Syl.Syl
    {Syl.onset = [], Syl.nucleus = [], Syl.coda = [], Syl.stress = NullStress}
