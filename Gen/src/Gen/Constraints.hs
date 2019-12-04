module Gen.Constraints where

import qualified Data.Map as Map
import Sound
import Sound.Syl

newtype Constraint =
  Constraint (Syl -> Bool)

makeCons :: Int -> String -> String -> Float -> String -> [[[[Constraint]]]]
makeCons lineCount meterS rhymeS rhymeThreshold customCons = undefined

makeRhymeMap :: String -> Map.Map Char Syl
makeRhymeMap rhymeString = undefined
