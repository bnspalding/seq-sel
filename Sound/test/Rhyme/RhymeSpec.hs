module Rhyme.RhymeSpec
  ( spec
  ) where

import Rhyme.Rhyme
import Test.Hspec

spec :: Spec
spec = do
  describe "Boolean comparisons" $ do
    describe "isRhyme" $ do
      it "checks equality between the rhymes of two syls" $ pending
    describe "isAssonance" $ do
      it "checks equality betwee the nuclei of two syls" $ pending
    describe "isAlliteration" $ do
      it "checks equality between the onsets of two syls" $ pending
  describe "Numerical comparisons" $ do
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
