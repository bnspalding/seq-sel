module Sound.Syllabify
  ( syllabify
  ) where

import Data.List
import Data.Maybe
import Data.Ord
import Sound
import Sound.Feature
import Sound.GenAm
import Sound.Stress

syllabify :: [Sound] -> Sound.Word
syllabify [] = []
syllabify ss = _syllabify [] [] Nothing ss

_syllabify :: [[Sound]] -> [Sound] -> Maybe SonorityDir -> [Sound] -> Sound.Word
-- Empty Case: do nothing when supplied an empty initial sound list
_syllabify _ _ _ [] = []
-- End Case: package up the result when the end of the list is reached
_syllabify result currentSyl prevDir [current] =
  let final =
        if prevDir == Just FLAT -- special case: FLAT at end creates new syllable
          then result ++ [currentSyl] ++ [[current]]
          else result ++ [currentSyl ++ [current]]
   in makeSyl <$> filter (not . null) final
-- Recursive Case: break the sound list into sublists at breakpoints
_syllabify result currentSyl prevDir (current:next:ss) =
  let currentDir =
        case current of
          (Sound "ˈ") -> Nothing
          (Sound "ˌ") -> Nothing
          _ -> Just (getDir current next)
      (_result, _currentSyl) =
        case current of
          (Sound "ˈ") -> (result ++ [currentSyl], [current])
          (Sound "ˌ") -> (result ++ [currentSyl], [current])
          _ ->
            if shouldBreak prevDir currentDir
              then (result ++ [currentSyl], [current])
              else (result, currentSyl ++ [current])
   in _syllabify _result _currentSyl currentDir (next : ss)

type Sonority = Int

sonority :: Sound -> Sonority
sonority s
  | s == Sound "s" = 0 -- "s" is a special case
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
    fs = fromMaybe (featureSet []) (features s)

data SonorityDir
  = UP
  | DOWN
  | FLAT
  deriving (Ord, Eq)

getDir :: Sound -> Sound -> SonorityDir
getDir s1 s2
  | sonority s1 == sonority s2 = FLAT
  | sonority s1 < sonority s2 = UP
  | otherwise = DOWN

-- syllables should be broken up at DOWN-to-UP inflection points
shouldBreak :: Maybe SonorityDir -> Maybe SonorityDir -> Bool
shouldBreak (Just FLAT) _ = True
shouldBreak (Just DOWN) (Just UP) = True
shouldBreak _ _ = False

makeSyl :: [Sound] -> Syl
makeSyl [] = error "no sounds passed to makeSyl"
makeSyl soundList =
  Syl
    { onset = before
    , nucleus = [mostSonorous]
    , coda = after
    , stress = stressFromMaybe stressSymMaybe
    }
  where
    (stressSymMaybe, ss) = extractStressSym soundList
    (mostSonorous, mostSonorousI) =
      maximumBy (comparing (sonority . fst)) (zip ss [0 ..])
    (before, after) =
      case splitAt mostSonorousI ss of
        (_before, []) -> (_before, [])
        (_before, _:_after) -> (_before, _after)

stressSymsIPA :: [String]
stressSymsIPA = [stressSymbolIPA, secondaryStressSymbolIPA]

extractStressSym :: [Sound] -> (Maybe Sound, [Sound])
extractStressSym s = (stressMaybe, stressRemoved)
  where
    stressMaybe =
      case stressExtracted of
        [] -> Nothing
        [x] -> Just x
        _ -> error "multiple stress symbols found in one syllable"
    stressExtracted = filter (\(Sound x) -> x `elem` stressSymsIPA) s
    stressRemoved = filter (`notElem` stressExtracted) s

stressFromMaybe :: Maybe Sound -> Stress
stressFromMaybe s =
  case s of
    Nothing -> Unstressed
    Just (Sound "ˈ") -> Stressed
    Just (Sound "ˌ") -> SecondaryStress
    _ -> error "unknown stress symbol"
