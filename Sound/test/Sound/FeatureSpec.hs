module Sound.FeatureSpec
  ( spec
  ) where

import qualified Data.Set as Set
import Sound.Feature
import qualified Sound.GenAm as GenAm
import Sound.Sound
import Test.Hspec

spec :: Spec
spec = do
  describe "Sound.Feature" $ do
    context "with GenAm: natural classes of features" $ do
      it "isStop: p t k b d ɡ" $
        (_filter isStop) `shouldBe` Set.fromList ["p", "t", "k", "b", "d", "ɡ"]
      it "isVoiced: m b v ð n d z l d͡ʒ ʒ ɹ j ŋ ɡ w" $
        (_filter isVoiced) `shouldBe`
        Set.fromList
          [ "m"
          , "b"
          , "v"
          , "ð"
          , "n"
          , "d"
          , "z"
          , "l"
          , "d͡ʒ"
          , "ʒ"
          , "ɹ"
          , "j"
          , "ŋ"
          , "ɡ"
          , "w"
          ]
      it "isFricative: f v θ ð s z ʃ ʒ h" $
        (_filter isFricative) `shouldBe`
        Set.fromList ["f", "v", "θ", "ð", "s", "z", "ʃ", "ʒ", "h"]
      it "isAffricate: t͡ʃ d͡ʒ" $
        (_filter isAffricate) `shouldBe` Set.fromList ["t͡ʃ", "d͡ʒ"]
      it "isNasal: m n ŋ" $
        (_filter isNasal) `shouldBe` Set.fromList ["m", "n", "ŋ"]
      it "isLateral: l" $ (_filter isLateral) `shouldBe` Set.fromList ["l"]
      it "isApproximant: l ɹ j ʍ w" $
        (_filter isApproximant) `shouldBe`
        Set.fromList ["l", "ɹ", "j", "ʍ", "w"]
      it "isGlide: j ʍ w" $
        (_filter isGlide) `shouldBe` Set.fromList ["j", "ʍ", "w"]
      it "isVowel: i ɪ ɛ æ ə ʌ ɑ u ʊ ɔ e͡ɪ a͡ɪ a͡ʊ o͡ʊ ɔ͡ɪ ɜ˞ ə˞" $
        (_filter isVowel) `shouldBe`
        Set.fromList
          [ "i"
          , "ɪ"
          , "ɛ"
          , "æ"
          , "ə"
          , "ʌ"
          , "ɑ"
          , "u"
          , "ʊ"
          , "ɔ"
          , "e͡ɪ"
          , "a͡ɪ"
          , "a͡ʊ"
          , "o͡ʊ"
          , "ɔ͡ɪ"
          , "ɜ˞"
          , "ə˞"
          ]
      it "isHighVowel: i ɪ u ʊ" $
        (_filter isHighVowel) `shouldBe` Set.fromList ["i", "ɪ", "u", "ʊ"]
      it "isMidVowel: ɛ ə ʌ ɔ e͡ɪ a͡ɪ o͡ʊ ɔ͡ɪ ɜ˞ ə˞" $
        (_filter isMidVowel) `shouldBe`
        Set.fromList
          ["ɛ", "ə", "ʌ", "ɔ", "e͡ɪ", "a͡ɪ", "o͡ʊ", "ɔ͡ɪ", "ɜ˞", "ə˞"]
      it "isLowVowel: æ ɑ a͡ʊ" $
        (_filter isLowVowel) `shouldBe` Set.fromList ["æ", "ɑ", "a͡ʊ"]
    it "all features in use" $ pending

_filter :: (FeatureSet -> Bool) -> Set.Set String
_filter f =
  Set.map (\(Sound x) -> x) $ Set.filter (\x -> f (getFeatures x)) GenAm.sounds

getFeatures :: Sound -> FeatureSet
getFeatures s =
  case GenAm.features s of
    Nothing -> Set.empty
    Just fs -> fs
