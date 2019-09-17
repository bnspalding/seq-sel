module Sound.Pronunciation where

import qualified Data.Map as Map
import Sound
import Sound.IPA
import Sound.Syllabify

type PronunciationTable = Map.Map String Sound.Word

fromList :: [(String, String)] -> PronunciationTable
fromList mappings = Map.fromList $ valToSounds <$> mappings
  where
    valToSounds (k, v) = (k, syllabify $ stringToIPASounds v)
