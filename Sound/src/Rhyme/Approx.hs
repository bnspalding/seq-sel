module Rhyme.Approx
  ( rhyme
  , assonance
  , alliteration
  , similarity
  ) where

import qualified Data.Set as Set
import Sound
import Sound.Feature
import qualified Sound.GenAm as GenAm
import qualified Sound.Syl as Syl

rhyme :: Syl -> Syl -> Float
rhyme = _similarity Syl.rhyme

assonance :: Syl -> Syl -> Float
assonance = _similarity Syl.nucleus

alliteration :: Syl -> Syl -> Float
alliteration = _similarity Syl.onset

similarity :: [Sound] -> [Sound] -> Float
similarity ss1 ss2 =
  fromIntegral (Set.size $ Set.intersection fs1 fs2) /
  fromIntegral (Set.size $ Set.union fs1 fs2)
  where
    fs1 = _merge $ _featuresOf ss1
    fs2 = _merge $ _featuresOf ss2

_similarity :: (Syl -> [Sound]) -> Syl -> Syl -> Float
_similarity f syl1 syl2 = similarity (f syl1) (f syl2)

_featuresOf :: [Sound] -> [FeatureSet]
_featuresOf ss = featuresOrEmpty . GenAm.features <$> ss

_merge :: [FeatureSet] -> FeatureSet
_merge = foldl Set.union Set.empty
