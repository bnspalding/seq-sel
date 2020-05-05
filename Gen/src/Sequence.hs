-- |
-- Module: Sequence
-- Description: sequence functions
-- Copyright: (c) 2020 Ben Spalding (bnspalding.com)
-- License: CC-BY-NC
-- Stability: experimental
--
-- Sequence defines the Seq type and exposes a collection of Seq functions that
-- are used to generate the sequences of Sequence, Selection.
module Sequence
  ( Seq,
  )
where

import Dictionary
import Spec

-- | Seq describes a function that generates a sequence of Terms from a Term,
-- optionally using the spec to generate the list (to be selected from according
-- to constraints during generation)
type Seq = (Spec -> Entry -> [Entry])
