module Gen
  ( poem
  , writePoem
  , makeSpec
  , Spec(..)
  , Seq
  , Term
  , Line
  , Stanza
  , Constraint
  ) where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Dictionary
import Gen.Constraints
import Sound
import Sound.Pronunciation

type Term = Entry -- a dictionary Entry

type Line = [Term]

type Stanza = [Line]

data Spec =
  Spec
    { specConstraints :: [[[[Constraint]]]] -- stanza [ line [ syl [Constraint]]]
    , wordsUsed :: [Term]
    , rhymeMap :: Map.Map Char Syl
    , dict :: Dictionary
    }

type Seq = (Term -> [Term])

poem :: Spec -> Seq -> String -> [Stanza]
poem spec seqF firstWord =
  let d = dict spec
      es = Set.filter (\e -> text e == firstWord) d
   in if Set.null es
        then error "the given word is not present in the dictionary"
        else _poem spec seqF [[[first es]]]

_poem :: Spec -> Seq -> [Stanza] -> [Stanza]
-- First Word Case: some special handling is required for the first word
-- (such as filling with a previous word to get the meter right)
-- also, need to verify that the given word does indeed exist in dict
_poem spec seqF [[[w]]] = undefined
-- Standard Case: add words to the poem until the spec is fully realized
_poem spec seqF stanzas = undefined

writePoem :: [Stanza] -> String
writePoem = joinStanzas
  where
    joinStanzas s = intercalate "\n\n" $ joinLines <$> s
    joinLines ls = unlines $ joinTerms <$> ls
    joinTerms ts = unwords $ show <$> ts

makeSpec :: Int -> String -> String -> String -> Float -> String -> Spec
makeSpec lineCount rhymeS meterS dictFile rThreshold customCons =
  let cs = makeCons lineCount meterS rhymeS rThreshold customCons
      rm = makeRhymeMap rhymeS
      d = makeDict dictFile
   in Spec
        { specConstraints = cs
        , wordsUsed = []
        , rhymeMap = makeRhymeMap rhymeS
        , dict = d
        }

makeDict :: String -> Dictionary
makeDict = undefined
