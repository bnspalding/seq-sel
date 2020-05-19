-- |
-- Module: Gen.Constraints
-- Description: syllable level constraints
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: CC-BY-NC
-- Stability: experimental
--
-- Gen.Constraints describes constraints and state management used in poem
-- generation. PoemState is essentially a way of capturing state across the course of
-- poem generation, as its componenets are updated as words are added to the
-- poem. The specConstraints are consumed during genration, whittled away with
-- the addition of words until the spec is fully satisfied.
module Selection.Constraints
  ( -- * State Management
    PoemState (..),

    -- * Rigor Levels
    RigorLevel (..),

    -- * Constraints
    Constraint,
    StateMod,
    SylCons,
    LineCons,
    StanzaCons,
    PoemCons,
    makeRhymeConstraint,
    makeMeterConstraint,

    -- * Testing
    printPoemCons,
  )
where

import qualified Data.Map as Map
import Dictionary
import qualified Rhyme.Approx as Approx
import qualified Rhyme.Strict as Strict
import Sound
import Sound.Stress

-- | A PoemState manages state during generating. It tracks words used during
-- generation, the constraint block that is reduced throughout generation, and a
-- rhymeMap for rhyme constraints. Beyond the syllable constraints that are
-- formally managed by the PoemState, it also helps to enforce certain larger-scale
-- constraints, such as preventing a word that has been previously used from
-- appearing again in the poem.
--
-- Gen.Constraints is largely meant to be used internally and interfaced with
-- through Gen.
data PoemState
  = PoemState
      { specConstraints :: PoemCons, -- stanza [ line [ syl [Constraint]]]
        wordsUsed :: [Entry],
        rhymeMap :: Map.Map Char Syl
      }

-- | RigorLevels establish several different levels of \"rigor\" at which
-- a "Constraint" can define its operation. For example, a rhyme constraint can
-- be specificed using different fractions of the initial rhyme threshold for
-- the different rigor levels.
data RigorLevel
  = High
  | Medium
  | Low
  | None
  deriving (Eq, Ord, Show)

-- | A Constraint is a function that evaluates a syllable, using state from the
-- PoemState as needed (mainly the rhymeMap)
type Constraint = (RigorLevel -> Syl -> PoemState -> Bool)

-- | LineCons are a list of SylCons
type LineCons = [SylCons]

-- | StanzaCons are a list of LineCons
type StanzaCons = [LineCons]

-- | The PoemCons are the list of all the StanzaCons
type PoemCons = [StanzaCons]

-- | A StateMod is a modification to be made to the PoemState. These are held as
-- functions rather than applied immediately because a term must satisfy all of
-- its constraints before being applied.
type StateMod = (PoemState -> Syl -> PoemState)

-- | each syllable has both a set of constraints, and a set of
-- spec modifications to be run when an entry satisifies those constraints
-- this allows for modifying the rhyme scheme when an entry is selected
-- to an empty rhyme constraint.
type SylCons = ([Constraint], [StateMod])

makeRhymeConstraint :: Char -> Float -> (Constraint, StateMod)
makeRhymeConstraint c rThreshold =
  let rhymeFunc = selectRhymeFunc rThreshold
      con rl syl spec =
        let rMapSyl = rhymeMap spec Map.!? c
         in case rMapSyl of
              Nothing -> True
              Just syl2 -> rhymeFunc rl syl syl2
      upd spec syl =
        let rMap = rhymeMap spec
         in case rMap Map.!? c of
              Nothing ->
                PoemState
                  { rhymeMap = Map.insert c syl rMap,
                    specConstraints = specConstraints spec,
                    wordsUsed = wordsUsed spec
                  }
              Just _ -> spec
   in (con, upd)

makeMeterConstraint :: Stress -> (Constraint, StateMod)
makeMeterConstraint s =
  let con rl syl _
        | rl == High =
          case s of
            Stressed -> isHighStress $ stress syl
            Unstressed -> isLowStress $ stress syl
            _ -> error "meter constraints are binary. change before continuing"
        | rl == Medium || rl == Low =
          case s of -- only constrain stress points
            Stressed -> True
            Unstressed -> isLowStress $ stress syl
            _ -> error "meter constraints are binary. change before continuing"
        | otherwise = True
      upd spec _ = spec -- no change to spec
   in (con, upd)

selectRhymeFunc :: Float -> (RigorLevel -> Syl -> Syl -> Bool)
selectRhymeFunc rThreshold
  | rThreshold < 1.0 = approxFunc
  | otherwise = strictFunc
  where
    strictFunc rl s1 s2
      | rl == High || rl == Medium = Strict.rhyme s1 s2
      | otherwise = True -- Low or None
    approxFunc rl s1 s2
      | rl == High = Approx.rhyme s1 s2 >= toRational rThreshold
      | rl == Medium = Approx.rhyme s1 s2 >= toRational (rThreshold * 0.85)
      | rl == Low = Approx.rhyme s1 s2 >= toRational (rThreshold * 0.7)
      | otherwise = True -- None

printPoemCons :: PoemCons -> String
printPoemCons stanzas = concat $ showStanza <$> stanzas
  where
    showStanza stanza = "[st:" ++ concat (showLine <$> stanza) ++ "] "
    showLine line = "[l:" ++ concat (showSyl <$> line) ++ "]"
    showSyl syl = "[s:" ++ show (length (fst syl)) ++ "]"
