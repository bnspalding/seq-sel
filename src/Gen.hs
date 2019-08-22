module Gen 
( poem,
  writePoem,
  Spec (..),
  Seq (..),
  PoemWord (..)
) where

import Constraints
import Sound
import qualified Data.Map as Map

data PoemWord = PoemWord
data Spec = Spec {
    specConstraints :: [[[[Constraint]]]], -- stanza [ line [ syl [Constraint]]]
    wordsUsed :: [PoemWord],
    rhymeMap :: Map.Map Char Sound
}
data Seq = Dict | Vector

poem :: Spec -> Seq -> [PoemWord] -> [PoemWord]
poem spec seq w = undefined

writePoem :: [PoemWord] -> String
writePoem ws = undefined
