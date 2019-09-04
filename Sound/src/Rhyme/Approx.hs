module Rhyme.Approx
  ( rhyme
  , assonance
  , alliteration
  , similarity
  ) where

import qualified Data.Set as Set
import Sound.Feature
import qualified Sound.GenAm as GenAm
import Sound.Sound
import qualified Sound.Syl as Syl

rhyme :: Syl.Syl -> Syl.Syl -> Float
rhyme syl1 syl2 = _similarity Syl.rhyme syl1 syl2

assonance :: Syl.Syl -> Syl.Syl -> Float
assonance syl1 syl2 = _similarity Syl.nucleus syl1 syl2

alliteration :: Syl.Syl -> Syl.Syl -> Float
alliteration syl1 syl2 = _similarity Syl.onset syl1 syl2

similarity :: [Sound] -> [Sound] -> Float
similarity ss1 ss2 =
  (fromIntegral (Set.size $ Set.intersection fs1 fs2)) /
  (fromIntegral (Set.size $ Set.union fs1 fs2))
  where
    fs1 = _merge $ _featuresOf ss1
    fs2 = _merge $ _featuresOf ss2

_similarity :: (Syl.Syl -> [Sound]) -> Syl.Syl -> Syl.Syl -> Float
_similarity f syl1 syl2 = similarity (f syl1) (f syl2)

_featuresOf :: [Sound] -> [FeatureSet]
_featuresOf ss = (featuresOrEmpty . GenAm.features) <$> ss

_merge :: [FeatureSet] -> FeatureSet
_merge fsS = foldl (Set.union) Set.empty fsS
