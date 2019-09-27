module Sound.Pronunciation where

import qualified Data.Map as Map
import Sound
import Sound.IPA
import Sound.Syllabify

--NOTE: using a map to a single Pronunciation means that instances like
-- read/read (ɹid/ɹɛd) are reduced to a single pronunciation
type PronunciationTable = Map.Map String Pronunciation

type Pronunciation = Sound.Word

fromList :: [(String, String)] -> PronunciationTable
fromList mappings = Map.fromList $ valToSounds <$> mappings
  where
    valToSounds (word, ipa) = (word, syllabify $ stringToIPASounds ipa)

lookup :: PronunciationTable -> String -> Maybe Pronunciation
lookup t s = Map.lookup s t
