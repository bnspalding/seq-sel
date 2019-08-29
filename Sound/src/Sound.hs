module Sound where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Feature

newtype Sound =
  Sound String
  deriving (Eq, Show, Ord)
