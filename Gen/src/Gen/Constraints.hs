module Gen.Constraints
  ( Spec(..)
  , Constraint
  , SpecMod
  , makeCons
  , makeRhymeMap
  ) where

import qualified Data.Map as Map
import Dictionary
import qualified Rhyme.Approx as Approx
import qualified Rhyme.Strict as Strict
import Sound

data Spec =
  Spec
    { specConstraints :: PoemCons -- stanza [ line [ syl [Constraint]]]
    , wordsUsed :: [Entry]
    , rhymeMap :: Map.Map Char Syl
    , dict :: Dictionary
    }

type Constraint = (Syl -> Spec -> Bool)

type SpecMod = (Spec -> Syl -> Spec)

-- each syllable has both a set of constraints, and a set of
-- spec modifications to be run when an entry satisifies those constraints
-- this allows for modifying the rhyme scheme when an entry is selected
-- to an empty rhyme constraint.
type SylCons = ([Constraint], [SpecMod])

type LineCons = [SylCons]

type StanzaCons = [LineCons]

type PoemCons = [StanzaCons]

type SylLoc = (Int, Int, Int) -- the [Stanza] [Line] [Syl] address of a Syl

makeCons :: Int -> String -> String -> Float -> String -> PoemCons
makeCons linesN meterS rhymeS rhymeThreshold customCons = undefined

makeRhymeMap :: String -> Map.Map Char Syl
makeRhymeMap rhymeString = undefined

stanzaCount :: PoemCons -> Int
stanzaCount = length

lineCount :: StanzaCons -> Int
lineCount = length

sylCount :: LineCons -> Int
sylCount = length

constraintsAt :: PoemCons -> SylLoc -> SylCons
constraintsAt p (stanzaI, lineI, sylI) = p !! stanzaI !! lineI !! sylI

addRhymeScheme :: PoemCons -> String -> PoemCons
addRhymeScheme p rhymeS = undefined

addMeterScheme :: PoemCons -> String -> PoemCons
addMeterScheme = undefined

addConstraintAt :: PoemCons -> SylLoc -> (Constraint, SpecMod) -> PoemCons
addConstraintAt p (stanzaI, lineI, sylI) c = undefined

makeRhymeConstraint :: Char -> Float -> (Constraint, SpecMod)
makeRhymeConstraint c rThreshold =
  let rhymeFunc = selectRhymeFunc rThreshold
      con syl spec =
        let rMapSyl = rhymeMap spec Map.!? c
         in case rMapSyl of
              Nothing -> True
              Just syl2 -> rhymeFunc syl syl2
      upd spec syl =
        let rMap = rhymeMap spec
         in case rMap Map.!? c of
              Nothing -> Spec {rhymeMap = Map.insert c syl rMap}
              Just _ -> spec
   in (con, upd)

makeMeterConstraint :: Stress -> (Constraint, SpecMod)
makeMeterConstraint s = undefined

selectRhymeFunc :: Float -> (Syl -> Syl -> Bool)
selectRhymeFunc rThreshold
  | rThreshold < 1.0 = \s1 s2 -> Approx.rhyme s1 s2 >= rThreshold
  | otherwise = Strict.rhyme
