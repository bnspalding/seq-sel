module Sound.FeatureSpec
  ( spec
  ) where

import Data.Maybe
import qualified Data.Set as Set
import Sound.Feature
import qualified Sound.GenAm as GenAm
import Sound.Sound
import Test.Hspec

spec :: Spec
spec =
  describe "Natural Classes from Features" $
  context "with the Sound.GenAm sound set" $ do
    it "isStop: p t k b d ɡ" $
      _filter isStop `shouldBe` Set.fromList ["p", "t", "k", "b", "d", "ɡ"]
    it "isVoiced: m b v ð n d z l d͡ʒ ʒ ɹ j ŋ ɡ w" $
      _filter isVoiced `shouldBe` Set.fromList allVoicedString
    it "isFricative: f v θ ð s z ʃ ʒ h" $
      _filter isFricative `shouldBe`
      Set.fromList ["f", "v", "θ", "ð", "s", "z", "ʃ", "ʒ", "h"]
    it "isAffricate: t͡ʃ d͡ʒ" $
      _filter isAffricate `shouldBe` Set.fromList ["t͡ʃ", "d͡ʒ"]
    it "isNasal: m n ŋ" $
      _filter isNasal `shouldBe` Set.fromList ["m", "n", "ŋ"]
    it "isLateral: l" $ _filter isLateral `shouldBe` Set.fromList ["l"]
    it "isApproximant: l ɹ j ʍ w" $
      _filter isApproximant `shouldBe` Set.fromList ["l", "ɹ", "j", "ʍ", "w"]
    it "isGlide: j ʍ w" $
      _filter isGlide `shouldBe` Set.fromList ["j", "ʍ", "w"]
    it "isVowel: i ɪ ɛ æ ə ʌ ɑ u ʊ ɔ e͡ɪ a͡ɪ a͡ʊ o͡ʊ ɔ͡ɪ ɜ˞ ə˞" $
      _filter isVowel `shouldBe` Set.fromList allVowelsString
    it "isHighVowel: i ɪ u ʊ" $
      _filter isHighVowel `shouldBe` Set.fromList ["i", "ɪ", "u", "ʊ"]
    it "isMidVowel: ɛ ə ʌ ɔ e͡ɪ a͡ɪ o͡ʊ ɔ͡ɪ ɜ˞ ə˞" $
      _filter isMidVowel `shouldBe`
      Set.fromList ["ɛ", "ə", "ʌ", "ɔ", "e͡ɪ", "a͡ɪ", "o͡ʊ", "ɔ͡ɪ", "ɜ˞", "ə˞"]
    it "isLowVowel: æ ɑ a͡ʊ" $
      _filter isLowVowel `shouldBe` Set.fromList ["æ", "ɑ", "a͡ʊ"]
    -- Note: this is not a necessary condition for Features, but it is an
    -- occasionally useful measure.
    -- it "all features in use" $ allFeaturesInUse `shouldBe` allFeatures

_filter :: (FeatureSet -> Bool) -> Set.Set String
_filter f =
  Set.map (\(Sound x) -> x) $ Set.filter (f . getFeatures) GenAm.sounds

-- allFeaturesInUse :: FeatureSet
-- allFeaturesInUse =
--   Set.foldl (\acc x -> Set.union acc (getFeatures x)) Set.empty GenAm.sounds
-- 
-- allFeatures :: FeatureSet
-- allFeatures =
--   Set.fromList
--     [ PLUS_SYLLABIC
--     , MINUS_SYLLABIC
--     , PLUS_CONSONANTAL
--     , MINUS_CONSONANTAL
--     , PLUS_SONORANT
--     , MINUS_SONORANT
--     , PLUS_CONTINUANT
--     , MINUS_CONTINUANT
--     , PLUS_VOICE
--     , MINUS_VOICE
--     , NASAL
--     , LATERAL
--     , DELREL
--     , SG
--     , CG
--     , LABIAL
--     , CORONAL
--     , DORSAL
--     , PHARYNGEAL
--     , LARYNGEAL
--     , PLUS_ANTERIOR
--     , MINUS_ANTERIOR
--     , PLUS_DISTRIB
--     , MINUS_DISTRIB
--     , PLUS_STRIDENT
--     , MINUS_STRIDENT
--     , PLUS_ROUND
--     , MINUS_ROUND
--     , PLUS_HIGH
--     , MINUS_HIGH
--     , PLUS_LOW
--     , MINUS_LOW
--     , PLUS_BACK
--     , MINUS_BACK
--     , PLUS_ATR
--     , MINUS_ATR
--     , PLUS_WIDE
--     , MINUS_WIDE
--     , RHOTIC
--     , PLUS_STRESSED
--     , MINUS_STRESSED
--     ]
getFeatures :: Sound -> FeatureSet
getFeatures s = fromMaybe Set.empty (GenAm.features s)

allVoicedString :: [String]
allVoicedString =
  ["m", "b", "v", "ð", "n", "d", "z", "l", "d͡ʒ", "ʒ", "ɹ", "j", "ŋ", "ɡ", "w"]

allVowelsString :: [String]
allVowelsString =
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
