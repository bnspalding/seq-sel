module Sound.Pronunciation where

import qualified Data.Map as Map
import Sound
import Sound.IPA
import Sound.Syllabify

type Pronunciation = Sound.Word

makePronunciation :: String -> Pronunciation
makePronunciation = syllabify . stringToIPASounds
