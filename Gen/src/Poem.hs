module Poem (Term, Line, Stanza, Poem) where

import Dictionary

-- | A Term is a word and its surrounding information, or, in other words, a
-- dictionary Entry
type Term = Entry

-- | A Line (as in a line of the output poem) is simply a list of Terms
type Line = [Term]

-- | A Stanza (as in a stanza of the output poem) is a list (grouping) of Lines
type Stanza = [Line]

-- | A Poem is a list (grouping) of Stanzas.
--
-- Poems in Sequence, Selection are fundamentally list poems. The reading of a
-- poem is conceptualized here as the enumeration of a list by the reader.
type Poem = [Stanza]
