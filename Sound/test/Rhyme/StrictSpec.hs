module Rhyme.StrictSpec
  ( spec
  ) where

import Rhyme.Strict
import Test.Hspec

spec :: Spec
spec = do
  describe "isRhyme" $ do
    it "checks equality between the rhymes of two syls" $ pending
  describe "isAssonance" $ do
    it "checks equality betwee the nuclei of two syls" $ pending
  describe "isAlliteration" $ do
    it "checks equality between the onsets of two syls" $ pending
