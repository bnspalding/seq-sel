module Sound.IPASpec
  ( spec
  ) where

import Sound
import Sound.IPA
import Test.Hspec

spec :: Spec
spec =
  describe "stringToIPASounds" $
  describe "test cases" $ do
    it "case: oʊ -> o͡ʊ" $
      stringToIPASounds "boʊ" `shouldBe` Sound <$> ["b", "o͡ʊ"]
    it "case: eɪ -> e͡ɪ" $
      stringToIPASounds "beɪ" `shouldBe` Sound <$> ["b", "e͡ɪ"]
    it "case: aɪ -> a͡ɪ" $
      stringToIPASounds "baɪ" `shouldBe` Sound <$> ["b", "a͡ɪ"]
    it "case: ɔɪ -> ɔ͡ɪ" $
      stringToIPASounds "bɔɪ" `shouldBe` Sound <$> ["b", "ɔ͡ɪ"]
    it "case: aʊ -> a͡ʊ" $
      stringToIPASounds "baʊ" `shouldBe` Sound <$> ["b", "a͡ʊ"]
    it "case: ɚ -> ə˞" $ stringToIPASounds "pɚ" `shouldBe` Sound <$> ["p", "ə˞"]
    it "case: ɝ -> ɜ˞" $ stringToIPASounds "pɝ" `shouldBe` Sound <$> ["p", "ɜ˞"]
    it "case: ɜɹ -> ɜ˞" $
      stringToIPASounds "pɜɹ" `shouldBe` Sound <$> ["p", "ɜ˞"]
    it "case: ə(ɹ) -> ə˞" $
      stringToIPASounds "pə(ɹ)" `shouldBe` Sound <$> ["p", "ə˞"]
    it "case: tʃ -> t͡ʃ" $
      stringToIPASounds "tʃiz" `shouldBe` Sound <$> ["t͡ʃ", "i", "z"]
    it "case: dʒ -> d͡ʒ" $
      stringToIPASounds "dʒinz" `shouldBe` Sound <$> ["d͡ʒ", "i", "n", "z"]
