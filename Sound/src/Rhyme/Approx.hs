module Rhyme.Approx
  ( rhyme
  , assonance
  , alliteration
  ) where

import qualified Data.Set as Set
import Sound.Feature
import qualified Sound.GenAm as GenAm
import Sound.Sound
import qualified Sound.Syl as Syl

rhyme :: Syl.Syl -> Syl.Syl -> Float
rhyme syl1 syl2 = _compare Syl.rhyme syl1 syl2

assonance :: Syl.Syl -> Syl.Syl -> Float
assonance syl1 syl2 = _compare Syl.nucleus syl1 syl2

alliteration :: Syl.Syl -> Syl.Syl -> Float
alliteration syl1 syl2 = _compare Syl.onset syl1 syl2

_compare :: (Syl.Syl -> [Sound]) -> Syl.Syl -> Syl.Syl -> Float
_compare f syl1 syl2 =
  (fromIntegral (Set.size $ Set.intersection fs1 fs2)) /
  (fromIntegral (Set.size $ Set.union fs1 fs2))
  where
    fs1 = _merge $ _featuresOf $ f syl1
    fs2 = _merge $ _featuresOf $ f syl2

_featuresOf :: [Sound] -> [FeatureSet]
_featuresOf ss = ((\(Just x) -> x) . GenAm.features) <$> ss

_merge :: [FeatureSet] -> FeatureSet
_merge fsS = foldl (Set.union) Set.empty fsS
