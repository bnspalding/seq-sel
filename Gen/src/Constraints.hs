module Constraints where

import Syl

newtype Constraint =
  Constraint (Syl -> Bool)
