module Gen 
( poem,
  writePoem,
  Spec (..),
  Seq (..),
  PoemWord (..)
) where

data PoemWord = PoemWord
data Spec = Spec
data Seq = Dict | Vector

poem :: Spec -> Seq -> [PoemWord] -> [PoemWord]
poem spec seq w = undefined

writePoem :: [PoemWord] -> String
writePoem ws = undefined
