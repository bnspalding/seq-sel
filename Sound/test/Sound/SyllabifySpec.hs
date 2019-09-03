module Sound.SyllabifySpec
  ( spec
  ) where

import Sound.Sound
import Sound.Stress
import Sound.Syl
import Sound.Syllabify
import Test.Hspec

spec :: Spec
spec = do
  describe "syllabify" $ do
    it "groups a list of sounds into syllables" $
      syllabify (Sound <$> ["ɹ", "ɪ", "p", "l", "a͡ɪ"]) `shouldBe`
      [ Syl
          { onset = [Sound ("ɹ")]
          , nucleus = [Sound ("ɪ")]
          , coda = []
          , stress = NullStress
          }
      , Syl
          { onset = [Sound ("p"), Sound ("l")]
          , nucleus = [Sound ("a͡ɪ")]
          , coda = []
          , stress = NullStress
          }
      ]
