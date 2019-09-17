module Rhyme.Strict where

import Sound
import qualified Sound.Syl as Syl

rhyme :: Syl -> Syl -> Bool
rhyme syl1 syl2 = Syl.rhyme syl1 == Syl.rhyme syl2

assonance :: Syl -> Syl -> Bool
assonance syl1 syl2 = Syl.nucleus syl1 == Syl.nucleus syl2

alliteration :: Syl -> Syl -> Bool
alliteration syl1 syl2 = Syl.onset syl1 == Syl.onset syl2
