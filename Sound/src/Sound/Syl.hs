module Sound.Syl where

import Sound.Sound
import Sound.Stress

data Syl =
  Syl
    { onset :: [Sound]
    , nucleus :: [Sound]
    , coda :: [Sound]
    , stress :: Stress
    }

rhyme :: Syl -> [Sound]
rhyme syl = nucleus syl ++ coda syl

sounds :: Syl -> [Sound]
sounds syl = onset syl ++ nucleus syl ++ coda syl