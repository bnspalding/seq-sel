module Rhyme.ApproxSpec
  ( spec
  ) where

import Rhyme.Approx
import Test.Hspec

spec :: Spec
spec = do
  describe "RhymeSimilarity" $ do
    it "measures the feature similarity between the rhymes of two syls" $
      pending
    it "is normalized between the numbers 0 and 1" $ pending
  describe "AssonanceSimilarity" $ do
    it "measures the feature similarity between the nuclei of two syls" $
      pending
    it "is normalized between the numbers 0 and 1" $ pending
  descirbe "AlliterationSimilarity" $ do
    it "measures the feature similarity between the onsets of two syls" $
      pending
    it "is normalized between the numbers 0 and 1" $ pending
