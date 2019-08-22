module Gen 
( poem,
  writePoem,
  Spec (..),
  Seq (..)
) where

data Spec = Spec
data Seq = Dict | Vector

poem :: Spec -> Seq -> String -> [Word]
poem spec seq w = undefined

writePoem :: [Word] -> String
writePoem ws = undefined
