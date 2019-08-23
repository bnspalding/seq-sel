module Gen 
( poem,
  writePoem,
  Spec (..),
  Seq (..),
  Term (..)
) where

import Constraints
import Sound
import qualified Data.Map as Map

data Term = Term
data Spec = Spec {
    specConstraints :: [[[[Constraint]]]], -- stanza [ line [ syl [Constraint]]]
    wordsUsed :: [Term],
    rhymeMap :: Map.Map Char Sound
}
data Seq = Dict | Vector

poem :: Spec -> Seq -> [Term] -> [Term]
poem spec seq w = undefined

writePoem :: [Term] -> String
writePoem ws = undefined
