module Gen
  ( poem
  , writePoem
  , Spec(..)
  , Seq
  , Term
  , Line
  , Stanza
  ) where

import Constraints
import Data.List
import qualified Data.Map as Map
import Dictionary
import Sound
import Sound.Pronunciation

-- unless things get overly complicated, Term should just be a String
type Term = String

type Line = [Term]

type Stanza = [Line]

data Spec =
  Spec
    { specConstraints :: [[[[Constraint]]]] -- stanza [ line [ syl [Constraint]]]
    , wordsUsed :: [Term]
    , rhymeMap :: Map.Map Char Sound
    , pronTable :: PronunciationTable
    , dict :: Dictionary
    }

type Seq = (Term -> [Term])

poem :: Spec -> Seq -> [Stanza] -> [Stanza]
-- First Word Case: some special handling is required for the first word
-- (such as filling with a previous word to get the meter right)
-- also, need to verify that the given word does indeed exist in dict
poem spec seq [[[w]]] = undefined
-- Standard Case: add words to the poem until the spec is fully realized
poem spec seq stanzas = undefined

writePoem :: [Stanza] -> String
writePoem = joinStanzas
  where
    joinStanzas s = intercalate "\n\n" $ joinLines <$> s
    joinLines ls = unlines $ joinTerms <$> ls
    joinTerms ts = unwords $ show <$> ts
