module Rhyme.StrictSpec
  ( spec
  ) where

import Rhyme.Strict
import Test.Hspec

spec :: Spec
spec = do
  describe "rhyme" $ do
    it "checks equality between the rhymes of two syls" $ pending
  describe "assonance" $ do
    it "checks equality betwee the nuclei of two syls" $ pending
  describe "alliteration" $ do
    it "checks equality between the onsets of two syls" $ pending
