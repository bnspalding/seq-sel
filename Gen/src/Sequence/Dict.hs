-- |
-- Module: Sequence.Dict
-- Description: dictionary sequence function
-- Copyright: (c) 2020 Ben Spalding (bnspalding.com)
-- License: CC-BY-NC
-- Stability: experimental
--
-- Sequence.Dict provides a sequence function for generating an infinite
-- sequence by marching alphabetically through a dictionary from a given
-- starting point.
module Sequence.Dict
  ( seqFunc,
  )
where

import Dictionary
import Spec

-- | seqFunc generates an infinite sequence by marching through a dictionary
-- alphabetically, using the dictionary associated with the 'Spec' and starting
-- with a given 'Entry'
seqFunc :: Spec -> Entry -> [Entry]
seqFunc spec e = dropWhile (/= e) $ cycle $ toList $ dict spec
