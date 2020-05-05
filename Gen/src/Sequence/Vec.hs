-- |
-- Module: Sequence.Vec
-- Description: word vectors sequence function
-- Copyright: (c) 2020 Ben Spalding (bnspalding.com)
-- License: CC-BY-NC
-- Stability: experimental
--
-- Sequence.Vec provides a sequence function for generating an infinite
-- sequence by hopping across links of semantic similarity between words, where
-- similarity is calculated from a set of vectors that represent the meaning of
-- the words by their co-occurence in a corpus.
module Sequence.Vec
  ( seqFunc,
  )
where

import Dictionary
import Spec

seqFunc :: Spec -> Entry -> [Entry]
seqFunc = undefined
-- Note: a special version of spec that has been modified with a set of word
-- vectors is probably required here. The seqFunc needs to be somehow
-- parameterized with the set of word vectors that are used to calculate
-- similarity (or at least access to that set of word vectors)

-- Note: The spec should be further modified to work with a theme word or
-- line-word that can provide a unifying base for semantic hops. Simply hopping
-- from one word to the next without a cohesive base is too incoherent
