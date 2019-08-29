module Word where

import Sound
import Syl

type Word = [Syl]

sounds :: Word.Word -> [Sound]
sounds w = foldl1 (++) $ Syl.sounds <$> w
