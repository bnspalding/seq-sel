module Constraints where

import Sound.Syl

newtype Constraint =
  Constraint (Syl -> Bool)
