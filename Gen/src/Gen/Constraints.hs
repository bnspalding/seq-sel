{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Gen.Constraints
-- Description: syllable level constraints
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: CC-BY-NC
-- Stability: experimental
--
-- Gen.Constraints describes the Specification and constraints used in poem
-- generation. Spec is essentially a way of capturing state across the course of
-- poem generation, as its componenets are updated as words are added to the
-- poem. The specConstraints are consumed during genration, whittled away with
-- the addition of words until the spec is fully satisfied.
module Gen.Constraints
  ( -- * Specifications
    Spec (..),
    makeCons,
    makeRhymeMap,

    -- * Constraints
    Constraint,
    SpecMod,
    SylCons,
    LineCons,
    StanzaCons,
    PoemCons,

    -- * Testing
    printPoemCons,
  )
where

import qualified Data.Map as Map
import qualified Data.Text as T
import Dictionary
import qualified Rhyme.Approx as Approx
import qualified Rhyme.Strict as Strict
import Sound

-- | A Spec manages state during generating. It tracks words used during
-- generation, the constraint block that is reduced throughout generation, and a
-- rhymeMap for rhyme constraints. Beyond the syllable constraints that are
-- formally managed by the Spec, it also helps to enforce certain larger-scale
-- constraints, such as preventing a word that has been previously used from
-- appearing again in the poem.
--
-- Gen.Constraints is largely meant to be used internally and interfaced with
-- through Gen.
data Spec
  = Spec
      { specConstraints :: PoemCons, -- stanza [ line [ syl [Constraint]]]
        wordsUsed :: [Entry],
        rhymeMap :: Map.Map Char Syl,
        dict :: Dictionary
      }

-- | A Constraint is a function that evaluates a syllable, using state from the
-- Spec as needed (mainly the rhymeMap)
type Constraint = (Syl -> Spec -> Bool)

-- | A SpecMod is a modification to be made to the Spec. These are held as
-- functions rather than applied immediately because a term must satisfy all of
-- its constraints before being applied.
type SpecMod = (Spec -> Syl -> Spec)

-- | each syllable has both a set of constraints, and a set of
-- spec modifications to be run when an entry satisifies those constraints
-- this allows for modifying the rhyme scheme when an entry is selected
-- to an empty rhyme constraint.
type SylCons = ([Constraint], [SpecMod])

-- | LineCons are a list of SylCons
type LineCons = [SylCons]

-- | StanzaCons are a list of LineCons
type StanzaCons = [LineCons]

-- | The PoemCons are the list of all the StanzaCons
type PoemCons = [StanzaCons]

type SylLoc = (Int, Int, Int) -- the [Stanza] [Line] [Syl] address of a Syl

-- emptyPoem :: PoemCons
-- emptyPoem = [emptyStanza]

emptyStanza :: StanzaCons
emptyStanza = [emptyLine]

emptyLine :: LineCons
emptyLine = [emptySyl]

emptySyl :: SylCons
emptySyl = ([], [])

-- | makeCons generates the poem constraints from a set of options
makeCons :: Int -> T.Text -> T.Text -> Float -> T.Text -> PoemCons
makeCons linesN meterS rhymeS rhymeThreshold customCons =
  let base = replicate linesN []
      withMeter = addMeterScheme base meterS
      withRhyme = addRhymeScheme withMeter rhymeS rhymeThreshold
      _final = addCustomCons withRhyme customCons
   in _final

-- | the rhyme map starts empty and is constructed during generation
-- the keys (a,b,c) are built into the syllable constraints
makeRhymeMap :: T.Text -> Map.Map Char Syl
makeRhymeMap _ = Map.empty

stanzaCount :: PoemCons -> Int
stanzaCount = length

lineCount :: StanzaCons -> Int
lineCount = length

sylCount :: LineCons -> Int
sylCount = length

totalLineCount :: PoemCons -> Int
totalLineCount p = sum $ lineCount <$> p

--constraintsAt :: PoemCons -> SylLoc -> SylCons
--constraintsAt p (stanzaI, lineI, sylI) = p !! stanzaI !! lineI !! sylI

-- not currently implemented
addCustomCons :: PoemCons -> T.Text -> PoemCons
addCustomCons p customConString
  | customConString == "" = p
  | otherwise = error "custom cons has not been implemented"

addRhymeScheme :: PoemCons -> T.Text -> Float -> PoemCons
addRhymeScheme _p rhymeS rThreshold = toRS_recursive _p rhymeSExtended (0, 0)
  where
    rhymeSExtended = take (totalLineCount _p) $ cycle (T.unpack rhymeS)
    toRS_recursive p [] _ = p
    toRS_recursive p r@(c : cs) (stanzaI, lineI)
      | lineI >= lineCount (p !! stanzaI) = toRS_recursive p r (stanzaI + 1, 0)
      | otherwise =
        let newP =
              addConToSyl
                (stanzaI, lineI, sylCount (p !! stanzaI !! lineI) - 1)
                p
                (makeRhymeConstraint c rThreshold)
         in toRS_recursive newP cs (stanzaI, lineI + 1)

addMeterScheme :: PoemCons -> T.Text -> PoemCons
addMeterScheme p meterS = mergePoems p (toMeterCons p meterS)

toMeterCons :: PoemCons -> T.Text -> PoemCons
toMeterCons startingP scheme = toMC_recursive (T.unpack scheme) startingP (0, 0, 0)
  where
    toMC_recursive [] p _ = p
    toMC_recursive (c : cs) p (st, ln, sy)
      | c == '0' =
        toMC_recursive
          cs
          (addConstraintAt p (st, ln, sy) (makeMeterConstraint Unstressed))
          (st, ln, sy + 1)
      | c == '1' =
        toMC_recursive
          cs
          (addConstraintAt p (st, ln, sy) (makeMeterConstraint Stressed))
          (st, ln, sy + 1)
      | c == '/' = toMC_recursive cs p (st, ln + 1, 0) -- linebreak
      | c == 's' = toMC_recursive cs p (st + 1, 0, 0) -- stanzabreak
      | otherwise = error $ "unknown symbol" ++ [c] ++ "in meter cons"

addConstraintAt :: PoemCons -> SylLoc -> (Constraint, SpecMod) -> PoemCons
addConstraintAt p loc@(stanzaI, lineI, sylI) c
  | stanzaI >= stanzaCount p = againWithAnotherStanza
  | lineI >= lineCount (p !! stanzaI) = againWithAnotherLine
  | sylI >= sylCount (p !! stanzaI !! lineI) = againWithAnotherSyl
  | otherwise = addConToSyl loc p c
  where
    againWithAnotherStanza = addConstraintAt (addEmptyStanza p) loc c
    againWithAnotherLine = addConstraintAt (addEmptyLine stanzaI p) loc c
    againWithAnotherSyl = addConstraintAt (addEmptySyl stanzaI lineI p) loc c

addEmptyStanza :: PoemCons -> PoemCons
addEmptyStanza p = p ++ [emptyStanza]

addEmptyLine :: Int -> PoemCons -> PoemCons
addEmptyLine stanzaI p = replace stanzaI modifiedStanza p
  where
    modifiedStanza = p !! stanzaI ++ [emptyLine]

addEmptySyl :: Int -> Int -> PoemCons -> PoemCons
addEmptySyl stanzaI lineI p = replace stanzaI modifiedStanza p
  where
    modifiedStanza = replace lineI modifiedLine (p !! stanzaI)
    modifiedLine = p !! stanzaI !! lineI ++ [emptySyl]

addConToSyl :: SylLoc -> PoemCons -> (Constraint, SpecMod) -> PoemCons
addConToSyl sylLoc@(stanzaI, lineI, sylI) p c
  | badSylLoc sylLoc p = error $ "addConToSyl badSylLoc: " ++ show sylLoc
  | otherwise = modifiedPoem
  where
    modifiedSyl = mergeCons (p !! stanzaI !! lineI !! sylI) c
    modifiedLine = replace sylI modifiedSyl (p !! stanzaI !! lineI)
    modifiedStanza = replace lineI modifiedLine (p !! stanzaI)
    modifiedPoem = replace stanzaI modifiedStanza p

mergeCons :: SylCons -> (Constraint, SpecMod) -> SylCons
mergeCons (cs, ms) (c, m) = (c : cs, m : ms)

mergePoems :: PoemCons -> PoemCons -> PoemCons
mergePoems = mergeF mergeStanzas

mergeStanzas :: StanzaCons -> StanzaCons -> StanzaCons
mergeStanzas = mergeF mergeLines

mergeLines :: LineCons -> LineCons -> LineCons
mergeLines = mergeF mergeSyls

mergeSyls :: SylCons -> SylCons -> SylCons
mergeSyls (c1s, m1s) (c2s, m2s) = (c1s ++ c2s, m1s ++ m2s)

mergeF :: (a -> a -> a) -> [a] -> [a] -> [a]
mergeF _ [] ys = ys
mergeF _ xs [] = xs
mergeF f (x : xs) (y : ys) = f x y : mergeF f xs ys

replace :: Int -> a -> [a] -> [a]
replace i x xs =
  case splitAt i xs of
    (before, []) -> before ++ [x]
    (before, _ : after) -> before ++ [x] ++ after

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
              Nothing ->
                Spec
                  { rhymeMap = Map.insert c syl rMap,
                    specConstraints = specConstraints spec,
                    dict = dict spec,
                    wordsUsed = wordsUsed spec
                  }
              Just _ -> spec
   in (con, upd)

makeMeterConstraint :: Stress -> (Constraint, SpecMod)
makeMeterConstraint s =
  let con syl _ = stress syl == s --does syl stress == specified stress
      upd spec _ = spec -- no change to spec
   in (con, upd)

selectRhymeFunc :: Float -> (Syl -> Syl -> Bool)
selectRhymeFunc rThreshold
  | rThreshold < 1.0 = \s1 s2 -> Approx.rhyme s1 s2 >= toRational rThreshold
  | otherwise = Strict.rhyme

badSylLoc :: SylLoc -> PoemCons -> Bool
badSylLoc (stanzaI, lineI, sylI) p
  | stanzaI >= length p = True
  | lineI >= length (p !! stanzaI) = True
  | sylI >= length (p !! stanzaI !! lineI) = True
  | otherwise = False

printPoemCons :: PoemCons -> String
printPoemCons = show . fmap (fmap (fmap (show . length)))
--TODO: constraints are not being generated properly. Make some tests for
--creation as a starter.
--It looks like currently, a line of 8 and a line of 6 are going, but not the
--next two lines (looks like there's three actually after, which is wrong)"
