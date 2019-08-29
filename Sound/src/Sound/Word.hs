module Sound.Word where

import Sound.Sound
import Sound.Stress
import Sound.Syl as Syl

type Word = [Syl]

sounds :: Sound.Word.Word -> [Sound]
sounds w = foldl1 (++) $ Syl.sounds <$> w

stress :: Sound.Word.Word -> [Stress]
stress w = Syl.stress <$> w
