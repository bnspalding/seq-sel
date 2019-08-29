module Sound where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Feature
import qualified Sounds.GenAm as GenAm

newtype Sound =
  Sound String
  deriving (Eq, Show, Ord)

features :: Sound -> Maybe FeatureSet
features s = Map.lookup s GenAm.sounds
