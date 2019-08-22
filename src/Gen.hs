module Gen 
( poem 
) where

data Spec = Spec
data Seq = Seq

poem :: Spec -> Seq -> [Word]
poem spec seq = undefined

writePoem :: [Word] -> String
writePoem w = undefined
