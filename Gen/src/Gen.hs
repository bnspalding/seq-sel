module Gen
  ( poem
  , writePoem
  , Spec(..)
  , Seq
  , Term(..)
  ) where

import Constraints
import qualified Data.Map as Map
import Sound

-- TODO: define an actual type for Gen.Term
data Term =
  Term

data Spec =
  Spec
    { specConstraints :: [[[[Constraint]]]] -- stanza [ line [ syl [Constraint]]]
    , wordsUsed :: [Term]
    , rhymeMap :: Map.Map Char Sound
    }

type Seq = (Term -> [Term])

poem :: Spec -> Seq -> [Term] -> [Term]
poem spec seq w = undefined

writePoem :: [Term] -> String
writePoem ws = undefined
