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
    valToSounds (word, ipa) = (word, syllabify $ stringToIPASounds ipa)

lookup :: PronunciationTable -> String -> Maybe Pronunciation
lookup t s = Map.lookup s t
