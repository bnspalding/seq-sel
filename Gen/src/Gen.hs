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
import Sound

-- unless things get overly complicated, Term should just be a String
type Term = String

type Line = [Term]

type Stanza = [Line]

data Spec =
  Spec
    { specConstraints :: [[[[Constraint]]]] -- stanza [ line [ syl [Constraint]]]
    , wordsUsed :: [Term]
    , rhymeMap :: Map.Map Char Sound
    }

type Seq = (Term -> [Term])

poem :: Spec -> Seq -> [Stanza] -> [Stanza]
poem spec seq w = undefined

writePoem :: [Stanza] -> String
writePoem = joinStanzas
  where
    joinStanzas s = intercalate "\n\n" $ joinLines <$> s
    joinLines ls = unlines $ joinTerms <$> ls
    joinTerms ts = unwords $ show <$> ts
