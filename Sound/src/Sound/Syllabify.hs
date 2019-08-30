module Sound.Syllabify
  ( syllabify
  ) where

import Data.List
import Data.Ord
import Sound.Feature
import Sound.GenAm
import Sound.Sound
import Sound.Syl
import Sound.Word as W

syllabify :: [Sound] -> W.Word
syllabify [current, next, ss] = undefined

type Sonority = Int

sonority :: Sound -> Sonority
sonority s
  | not (isVoiced fs) && isStop fs = 1
  | isVoiced fs && isStop fs = 2
  | not (isVoiced fs) && isFricative fs = 3
  | isVoiced fs && isFricative fs = 4
  | isAffricate fs = 4
  | isNasal fs = 5
  | isLateral fs = 6
  | isApproximant fs || isGlide fs = 7
  | isHighVowel fs = 8
  | isMidVowel fs = 9
  | isLowVowel fs = 10
  | otherwise = 0
  where
    fs =
      case features s of
        Just f -> f
        Nothing -> featureSet []

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
