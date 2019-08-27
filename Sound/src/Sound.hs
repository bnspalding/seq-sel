module Sound where

import Data.Set as Set
import Feature

newtype Sound =
  Sound String
  deriving (Eq, Show, Ord)

features :: Sound -> FeatureSet
features s = undefined
