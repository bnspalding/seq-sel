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

sounds :: Syl -> [Sound]
sounds syl = onset syl ++ nucleus syl ++ coda syl
