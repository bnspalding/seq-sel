module Sound.Syllabify
  ( syllabify
  ) where

import Data.List
import Data.Ord
import Sound.Sound
import Sound.Syl
import Sound.Word as W

syllabify :: [Sound] -> W.Word
syllabify [current, next, ss] = undefined

type Sonority = Int

sonority :: Sound -> Sonority
sonority s = undefined

data SonorityDirection
  = UP
  | DOWN
  deriving (Ord, Eq)

getDir :: Sound -> Sound -> SonorityDirection
getDir s1 s2 =
  if (sonority s1) < (sonority s2)
    then UP
    else DOWN

-- syllables should be broken up at DOWN-to-UP inflection points
shouldBreak :: SonorityDirection -> Sound -> Sound -> Bool
shouldBreak prevDir s1 s2 =
  if prevDir == DOWN && curDir == UP
    then True
    else False
  where
    curDir = getDir s1 s2

makeSyl :: [Sound] -> Syl
makeSyl ss = Syl {onset = before, nucleus = [mostSonorous], coda = after}
  where
    (mostSonorous, mostSonorousI) =
      maximumBy (comparing (sonority . fst)) (zip ss [0 ..])
    (before, _:after) = splitAt mostSonorousI ss
