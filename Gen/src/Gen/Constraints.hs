module Gen.Constraints
  ( Spec(..)
  , Constraint
  , SpecMod
  , makeCons
  , makeRhymeMap
  ) where

import qualified Data.Map as Map
import Dictionary
import Sound

data Spec =
  Spec
    { specConstraints :: PoemCons -- stanza [ line [ syl [Constraint]]]
    , wordsUsed :: [Entry]
    , rhymeMap :: Map.Map Char Syl
    , dict :: Dictionary
    }

type Constraint = (Syl -> Spec -> Bool)

type SpecMod = (Spec -> Entry -> Spec)

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

addConstraintAt :: PoemCons -> SylLoc -> Constraint -> PoemCons
addConstraintAt p (stanzaI, lineI, sylI) c = undefined

--note: somehow, this needs to get ahold of the Spec's rhymeMap
-- the best way to do this might be to change Constraint's type to
-- (Syl -> Spec -> Bool) so that it just has all the info
makeRhymeConstraint :: Char -> Constraint
makeRhymeConstraint c = undefined

makeMeterConstraint :: Stress -> Constraint
makeMeterConstraint s = undefined
