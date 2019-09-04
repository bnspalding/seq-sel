module Sound.SylSpec
  ( spec
  ) where

import Sound.Sound
import Sound.Stress
import Sound.Syl
import Test.Hspec

spec :: Spec
spec =
  describe "Syl" $ do
    it "onset 'p l ɔ p s' -> [p, l]" $
      onset demoSyl `shouldBe` (Sound <$> ["p", "l"])
    it "nucleus 'p l ɔ p s' -> [ɔ]" $
      nucleus demoSyl `shouldBe` (Sound <$> ["ɔ"])
    it "coda 'p l ɔ p s' -> [p, s]" $
      coda demoSyl `shouldBe` (Sound <$> ["p", "s"])
    it "rhyme 'p l ɔ p s' -> [ɔ, p, s]" $
      rhyme demoSyl `shouldBe` (Sound <$> ["ɔ", "p", "s"])
    it "sounds 'p l ɔ p s' -> [p, l, ɔ, p, s]" $
      sounds demoSyl `shouldBe` (Sound <$> ["p", "l", "ɔ", "p", "s"])
    it "stress 'p l ɔ p s' -> Stressed" $ stress demoSyl `shouldBe` Stressed

demoSyl :: Syl
demoSyl =
  Syl
    { onset = Sound <$> ["p", "l"]
    , nucleus = Sound <$> ["ɔ"]
    , coda = Sound <$> ["p", "s"]
    , stress = Stressed
    }
