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
    RigorLevel (..),
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
import Sound.Stress

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
-- Spec as needed (mainly the rhymeMap)
type Constraint = (RigorLevel -> Syl -> Spec -> Bool)

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

-- | makeCons generates the poem constraints from a set of options
makeCons :: Int -> T.Text -> T.Text -> Float -> T.Text -> PoemCons
makeCons linesN meterS rhymeS rhymeThreshold customCons =
  let base = [replicate linesN []] -- this is a flat set of empty lines in 1 stanza
          -- stanza breaks must be introduced later (such as in 'withMeter')
      withMeter = addMeterScheme base meterS
      withRhyme = addRhymeScheme withMeter rhymeS rhymeThreshold
      _final = addCustomCons withRhyme customCons
   in _final

-- | the rhyme map starts empty and is constructed during generation
-- the keys (a,b,c) are built into the syllable constraints
makeRhymeMap :: T.Text -> Map.Map Char Syl
makeRhymeMap _ = Map.empty

-- stanzaCount :: PoemCons -> Int
-- stanzaCount = length

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

-- Because the meter scheme can fundamentally alter the structure of a poem
-- (inserting stanza breaks into an otherwise flat set of lines), 'mergePoems'
-- is not an appropriate tool. 'toMeterCons' must instead work over the given
-- PoemCons
addMeterScheme :: PoemCons -> T.Text -> PoemCons
addMeterScheme [] = error "empty poem cons in addMeterScheme"
addMeterScheme [stanzaCons] = toMeterCons stanzaCons
addMeterScheme _ = error "addMeterScheme expects a single stanza (flat set of lines)"

toMeterCons :: StanzaCons -> T.Text -> PoemCons
toMeterCons startingS scheme = toMC_recursive startingS extendedScheme True []
  where
    lc = lineCount startingS -- assume flat (structure comes with meter)
        -- for now, nothing is being done to filter out double line breaks or
        -- anything of that sort. This should be addressed in the future.
    lineBreak = "/"
    -- for now, stanzaBreaks flip an end-of-line switch. Consider changing this
    -- in the future.
    stanzaBreak = 's'
    -- a meterScheme is split by lines, and then extended to the length of the
    -- poem (if it is longer, it is reduced to the line length of the poem)
    extendedScheme = take lc $ cycle $ T.splitOn lineBreak scheme
    toMC_recursive p [] _ [] = [p]
    toMC_recursive _ [] _ newP = newP
    toMC_recursive [] _ _ _ = error "empty stanzaCons given, toMC_recursive"
    toMC_recursive (pl : pls) (l : ls) isBreak newP
      | isBreak =
        let (newL, shouldBreak) = processL pl l
         in toMC_recursive pls ls shouldBreak (newP ++ [[newL]])
      | otherwise =
        let (newL, shouldBreak) = processL pl l
         in toMC_recursive pls ls shouldBreak (init newP ++ [last newP ++ [newL]])
    processL oldLine mLine = (newL, shouldBreak)
      where
        newL = zipSyls oldLine $ genConstraint <$> T.unpack (T.filter (/= stanzaBreak) mLine)
        shouldBreak = not $ T.null $ T.filter (== stanzaBreak) mLine
    genConstraint c
      | c == '0' = let (con, m) = makeMeterConstraint Unstressed in ([con], [m])
      | c == '1' = let (con, m) = makeMeterConstraint Stressed in ([con], [m])
      | otherwise = error $ "unknown symbol " ++ [c] ++ "in meter scheme"

--like zipWith, but fills in with the longer list rather than trimming to the
--shorter
zipSyls :: LineCons -> LineCons -> LineCons
zipSyls [] [] = []
zipSyls [] yl = yl
zipSyls xl [] = xl
zipSyls (x : xs) (y : ys) = mergeSyls x y : zipSyls xs ys

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

mergeSyls :: SylCons -> SylCons -> SylCons
mergeSyls (c1s, m1s) (c2s, m2s) = (c1s ++ c2s, m1s ++ m2s)

replace :: Int -> a -> [a] -> [a]
replace i x xs =
  case splitAt i xs of
    (before, []) -> before ++ [x]
    (before, _ : after) -> before ++ [x] ++ after

makeRhymeConstraint :: Char -> Float -> (Constraint, SpecMod)
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

badSylLoc :: SylLoc -> PoemCons -> Bool
badSylLoc (stanzaI, lineI, sylI) p
  | stanzaI >= length p = True
  | lineI >= length (p !! stanzaI) = True
  | sylI >= length (p !! stanzaI !! lineI) = True
  | otherwise = False

printPoemCons :: PoemCons -> String
printPoemCons stanzas = concat $ showStanza <$> stanzas
  where
    showStanza stanza = "[st:" ++ concat (showLine <$> stanza) ++ "] "
    showLine line = "[l:" ++ concat (showSyl <$> line) ++ "]"
    showSyl syl = "[s:" ++ show (length (fst syl)) ++ "]"
