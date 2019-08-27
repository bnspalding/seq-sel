module Feature where

import Data.Set as Set

data Feature
  = SYLLABIC
  | NON_SYLLABIC

type FeatureSet = Set Feature
