module Sound.IPA
  ( stringToIPASounds
  , normalize
  ) where

import Data.List
import Data.Ord
import qualified Data.Set as Set
import Sound
import qualified Sound.GenAm as GenAm
import Sound.Stress
import Text.Replace

stringToIPASounds :: String -> [Sound]
stringToIPASounds s =
  let toIPAIter sounds [] = sounds
      toIPAIter sounds xs = toIPAIter (sounds ++ [sound]) remaining
        where
          (sound, remaining) = nextSound xs ipaSymbols
   in toIPAIter [] (normalize s)

normalize :: String -> String
normalize = replaceWithTrie repls . replaceWithTrie repls

repls :: Trie
repls =
  listToTrie
    [ Replace (s "oʊ") "o͡ʊ"
    , Replace (s "eɪ") "e͡ɪ"
    , Replace (s "aɪ") "a͡ɪ"
    , Replace (s "ɔɪ") "ɔ͡ɪ"
    , Replace (s "aʊ") "a͡ʊ"
    , Replace (s "(ɹ)") "ɹ"
    , Replace (s "ɚ") "ə˞"
    , Replace (s "ɝ") "ɜ˞"
    , Replace (s "ɜɹ") "ɜ˞"
    , Replace (s "əɹ") "ə˞"
    , Replace (s "tʃ") "t͡ʃ"
    , Replace (s "dʒ") "d͡ʒ"
    , Replace (s " ") ""
    , Replace (s "\r") ""
    , Replace (s "\t") ""
    , Replace (s "\n") ""
    ]
  where
    s = string'fromString

stressSymbols :: [String]
stressSymbols = [stressSymbolIPA, secondaryStressSymbolIPA]

ipaSymbols :: [String]
ipaSymbols =
  (sortOn (Down . length) .
   (++) stressSymbols . Set.toList . Set.map (\(Sound x) -> x))
    GenAm.sounds

nextSound :: String -> [String] -> (Sound, String)
nextSound [] _ = error "empty list given to nextSound in Sound.IPA"
nextSound (x:xs) [] = error $ "unknown symbol " ++ [x] ++ " in " ++ xs
nextSound xs (sym:syms) =
  if sym `isPrefixOf` xs
    then (Sound sym, drop (length sym) xs)
    else nextSound xs syms
