module Sound.SyllabifySpec
  ( spec
  ) where

import Sound.Sound
import Sound.Stress
import Sound.Syl
import Sound.Syllabify
import Test.Hspec

spec :: Spec
spec =
  describe "syllabify" $ do
    describe "test cases" $ do
      it "case: ɹɪpla͡ɪ -> ɹɪ-pla͡ɪ" $
        syllabify (Sound <$> ["ɹ", "ɪ", "p", "l", "a͡ɪ"]) `shouldBe`
        [s ["ɹ"] ["ɪ"] [], s ["p", "l"] ["a͡ɪ"] []]
      it "case: ɹɛpta͡ɪl -> ɹɛp-ta͡ɪl" $
        syllabify (Sound <$> ["ɹ", "ɛ", "p", "t", "a͡ɪ", "l"]) `shouldBe`
        [s ["ɹ"] ["ɛ"] ["p"], s ["t"] ["a͡ɪ"] ["l"]]
      it "case: ɹɛplɪke͡ɪt -> ɹɛ-plɪ-ke͡ɪt" $
        syllabify (Sound <$> ["ɹ", "ɛ", "p", "l", "ɪ", "k", "e͡ɪ", "t"]) `shouldBe`
        [s ["ɹ"] ["ɛ"] [], s ["p", "l"] ["ɪ"] [], s ["k"] ["e͡ɪ"] ["t"]]
      it "case: wɪlmɪŋtʌn -> wɪl-mɪŋ-tʌn" $
        syllabify (Sound <$> ["w", "ɪ", "l", "m", "ɪ", "ŋ", "t", "ʌ", "n"]) `shouldBe`
        [s ["w"] ["ɪ"] ["l"], s ["m"] ["ɪ"] ["ŋ"], s ["t"] ["ʌ"] ["n"]]
      it "case: kʌnstɹe͡ɪnt -> kʌn-stɹe͡ɪnt" $
        syllabify (Sound <$> ["k", "ʌ", "n", "s", "t", "ɹ", "e͡ɪ", "n", "t"]) `shouldBe`
        [s ["k"] ["ʌ"] ["n"], s ["s", "t", "ɹ"] ["e͡ɪ"] ["n", "t"]]
      it "case: wɛstə˞n -> wɛ-stə˞n" $
        syllabify (Sound <$> ["w", "ɛ", "s", "t", "ə˞", "n"]) `shouldBe`
        [s ["w"] ["ɛ"] [], s ["s", "t"] ["ə˞"] ["n"]]
      it "case: pænts -> pænts" $
        syllabify (Sound <$> ["p", "æ", "n", "t", "s"]) `shouldBe`
        [s ["p"] ["æ"] ["n", "t", "s"]]
      it "case: skwɜ˞əl -> skwɜ˞-əl" $
        syllabify (Sound <$> ["s", "k", "w", "ɜ˞", "ə", "l"]) `shouldBe`
        [s ["s", "k", "w"] ["ɜ˞"] [], s [] ["ə"] ["l"]]
      it "case: læf -> læf" $
        syllabify (Sound <$> ["l", "æ", "f"]) `shouldBe` [s ["l"] ["æ"] ["f"]]
      it "case: pɹɛʃə˞ -> pɹɛ-ʃə˞" $
        syllabify (Sound <$> ["p", "ɹ", "ɛ", "ʃ", "ə˞"]) `shouldBe`
        [s ["p", "ɹ"] ["ɛ"] [], s ["ʃ"] ["ə˞"] []]
      it "case: ɹɛt͡ʃɪd -> ɹɛ-t͡ʃɪd" $
        syllabify (Sound <$> ["ɹ", "ɛ", "t͡ʃ", "ɪ", "d"]) `shouldBe`
        [s ["ɹ"] ["ɛ"] [], s ["t͡ʃ"] ["ɪ"] ["d"]]
      it "case: pɑɑ -> pɑ-ɑ" $
        syllabify (Sound <$> ["p", "ɑ", "ɑ"]) `shouldBe`
        [s ["p"] ["ɑ"] [], s [] ["ɑ"] []]
      it "case: pɑɑl -> pɑ-ɑl" $
        syllabify (Sound <$> ["p", "ɑ", "ɑ", "l"]) `shouldBe`
        [s ["p"] ["ɑ"] [], s [] ["ɑ"] ["l"]]
    describe "empty list" $
      it "returns an empty syl set" $ syllabify [] `shouldBe` []
    describe "single sound" $
      it "works as expected" $ syllabify [Sound "t"] `shouldBe` [s [] ["t"] []]
    describe "two sounds" $
      it "works as expected" $
      syllabify (Sound <$> ["l", "p"]) `shouldBe` [s [] ["l"] ["p"]]
    describe "two equal sounds" $ do
      context "with vowels" $
        it "splits them into two syllables" $
        syllabify (Sound <$> ["a͡ɪ", "a͡ɪ"]) `shouldBe`
        [s [] ["a͡ɪ"] [], s [] ["a͡ɪ"] []]
      context "with consonants" $
        it "splits them into two syllables" $
        syllabify (Sound <$> ["p", "p"]) `shouldBe`
        [s [] ["p"] [], s [] ["p"] []]
    describe "unknown input" $
      it "silently accepts the unknown symbols as sonority 0" $
      syllabify (Sound <$> ["p", "2", "ɑ"]) `shouldBe`
      [s [] ["p"] [], s ["2"] ["ɑ"] []]

s :: [String] -> [String] -> [String] -> Syl
s _onset _nucleus _coda =
  Syl
    { onset = Sound <$> _onset
    , nucleus = Sound <$> _nucleus
    , coda = Sound <$> _coda
    , stress = Unstressed
    }
