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
syllabify [] = []
syllabify ss = _syllabify [] [] Nothing ss

_syllabify ::
     [[Sound]] -> [Sound] -> Maybe SonorityDirection -> [Sound] -> W.Word
_syllabify _ _ _ [] = []
_syllabify result currentSyl _ (current:[]) =
  let final = (result ++ [currentSyl ++ [current]])
   in makeSyl <$> final
_syllabify result currentSyl prevDir (current:next:ss) =
  let currentDir = Just (getDir current next)
   in case shouldBreak prevDir currentDir of
        True ->
          _syllabify (result ++ [currentSyl]) [current] currentDir (next : ss)
        False ->
          _syllabify result (currentSyl ++ [current]) currentDir (next : ss)

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
getDir s1 s2
  | (sonority s1) < (sonority s2) = UP
  | otherwise = DOWN

-- syllables should be broken up at DOWN-to-UP inflection points
shouldBreak :: Maybe SonorityDirection -> Maybe SonorityDirection -> Bool
shouldBreak (Just DOWN) (Just UP) = True
shouldBreak _ _ = False

makeSyl :: [Sound] -> Syl
makeSyl ss = Syl {onset = before, nucleus = [mostSonorous], coda = after}
  where
    (mostSonorous, mostSonorousI) =
      maximumBy (comparing (sonority . fst)) (zip ss [0 ..])
    (before, _:after) = splitAt mostSonorousI ss
