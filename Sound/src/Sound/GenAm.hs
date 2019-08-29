module Sound.GenAm
  ( sounds
  , features
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Sound.Feature
import qualified Sound.GenAm.Sounds as GenAm
import Sound.Sound

sounds :: Set.Set Sound
sounds = Map.keysSet GenAm._sounds

features :: Sound -> Maybe FeatureSet
features s = Map.lookup s GenAm._sounds
