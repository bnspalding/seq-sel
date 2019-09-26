module Sound.Pronunciation where

import qualified Data.Map as Map
import Sound
import Sound.IPA
import Sound.Syllabify

type PronunciationTable = Map.Map String Pronunciation

type Pronunciation = Sound.Word

fromList :: [(String, String)] -> PronunciationTable
fromList mappings = Map.fromList $ valToSounds <$> mappings
  where
    valToSounds (k, v) = (k, syllabify $ stringToIPASounds v)

-- stripStress and readStress are currently rolled into syllabify
-- at some point in the future it may make sense to pull them out
-- but for now it's not necessary
-- Sound.Word already has sounds() and stress() to access this information
-- stripStress :: Pronunciation -> Pronunciation
-- stripStress = 
-- 
-- readStress :: Pronunciation -> [Stress]
-- readStress = -- march over vowels and adjacent stress marks (if present)
lookup :: PronunciationTable -> String -> Maybe Pronunciation
lookup t s = Map.lookup s t
