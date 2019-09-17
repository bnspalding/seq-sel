module Dictionary where

import qualified Data.Set as Set

type Dictionary = Set.Set String

fromList :: [String] -> Dictionary
fromList = Set.fromList

first :: Dictionary -> String
first = Set.elemAt 0

last :: Dictionary -> String
last d = Set.elemAt (size d - 1) d

size :: Dictionary -> Int
size = Set.size

next :: Dictionary -> String -> String
next d entry = Set.elemAt i d
  where
    prevI = Set.findIndex entry d
    i =
      if prevI == size d - 1
        then 0
        else prevI + 1

prev :: Dictionary -> String -> String
prev d entry = Set.elemAt i d
  where
    nextI = Set.findIndex entry d
    i =
      if nextI == 0
        then size d - 1
        else nextI - 1

contains :: Dictionary -> String -> Bool
contains d entry = Set.member entry d

-- NOTE: this currently will return the first word after some character,
-- so it will do things like firstOfLetter 'e' = "f..." if there are
-- no e words in the given dictionary
firstOfLetter :: Dictionary -> Char -> Maybe String
firstOfLetter d c = Set.lookupGE [c] d
