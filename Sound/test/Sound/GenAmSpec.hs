module Sound.GenAmSpec
  ( spec
  ) where

import qualified Data.Set as Set
import qualified Sound.GenAm as GenAm
import Sound.Sound
import Test.Hspec

spec :: Spec
spec =
  describe "data correctness" $ do
    it "consonants are: m n ŋ p b t d k ɡ t͡ʃ d͡ʒ f v θ ð s z ʃ ʒ h l ɹ j ʍ w" $
      GenAm.sounds `shouldSatisfy` Set.isProperSubsetOf consonants
    it "vowels are: i ɪ ɛ æ ə ʌ ɑ u ʊ ɔ e͡ɪ a͡ɪ a͡ʊ o͡ʊ ɔ͡ɪ ɜ˞ ə˞" $
      GenAm.sounds `shouldSatisfy` Set.isProperSubsetOf vowels
    it "does not contain any other symbols than those given above" $
      GenAm.sounds `shouldSatisfy` (== allSounds)
    it "does not contain common mistypes: e a o r g ɑ͡ɪ ɛ˞ y" $
      GenAm.sounds `shouldSatisfy` Set.disjoint mistypes
    it "contains no duplicate sounds (by feature set)" $
      GenAm.sounds `shouldSatisfy`
      foldl (\acc x -> acc && uniqueFeatureSet GenAm.sounds x) True

allSounds :: Set.Set Sound
allSounds = Set.union vowels consonants

uniqueFeatureSet :: Set.Set Sound -> Sound -> Bool
uniqueFeatureSet set s =
  foldl (\acc x -> acc && ((fs /= GenAm.features x) || s == x)) True set
  where
    fs = GenAm.features s

consonants :: Set.Set Sound
consonants =
  Set.fromList
    (Sound <$>
     [ "m"
     , "n"
     , "ŋ"
     , "p"
     , "b"
     , "t"
     , "d"
     , "k"
     , "ɡ"
     , "t͡ʃ"
     , "d͡ʒ"
     , "f"
     , "v"
     , "θ"
     , "ð"
     , "s"
     , "z"
     , "ʃ"
     , "ʒ"
     , "h"
     , "l"
     , "ɹ"
     , "j"
     , "ʍ"
     , "w"
     ])

vowels :: Set.Set Sound
vowels =
  Set.fromList
    (Sound <$>
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
     ])

mistypes :: Set.Set Sound
mistypes = Set.fromList (Sound <$> ["e", "a", "o", "r", "g", "ɑ͡ɪ", "ɛ˞", "y"])
