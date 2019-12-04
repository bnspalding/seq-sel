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

import Constraints
import Data.List
import qualified Data.Map as Map
import Dictionary
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

poem :: Spec -> Seq -> [Stanza] -> [Stanza]
-- First Word Case: some special handling is required for the first word
-- (such as filling with a previous word to get the meter right)
-- also, need to verify that the given word does indeed exist in dict
poem spec seqF [[[w]]] = undefined
-- Standard Case: add words to the poem until the spec is fully realized
poem spec seqF stanzas = undefined

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
