module Syl where

import Sound

data Syl =
  Syl
    { onset :: [Sound]
    , nucleus :: [Sound]
    , coda :: [Sound]
    }

rhyme :: Syl -> [Sound]
rhyme syl = nucleus syl ++ coda syl
