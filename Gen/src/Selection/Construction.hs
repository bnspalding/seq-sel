{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Selection.Construction
-- Description: generating constraint grids
-- Copyright: (c) 2020 Ben Spalding (bnspalding.com)
-- License: CC-BY-NC
-- Stability: experimental
--
-- Selection.Construction provides functions for layering constraints over a
-- grid of Stanzas(Lines(Syllables))). Construction is responsible for producing
-- the PoemState that is used in Gen, combining general constraints specified in
-- the poem's config file with arbitrary constraints specified in a .SSRS file.
module Selection.Construction
  ( makeCons,
    makeRhymeMap,
  )
where

import qualified Data.Map as Map -- REFACTOR: use Map.Strict instead?
import qualified Data.Text as T
import Selection.Constraints
import Sound

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
--
-- REFACTOR: This should no longer take an argument (or something needs to
-- change)
makeRhymeMap :: T.Text -> Map.Map Char Syl
makeRhymeMap _ = Map.empty

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

type SylLoc = (Int, Int, Int) -- the [Stanza] [Line] [Syl] address of a Syl

lineCount :: StanzaCons -> Int
lineCount = length

sylCount :: LineCons -> Int
sylCount = length

totalLineCount :: PoemCons -> Int
totalLineCount p = sum $ lineCount <$> p

--like zipWith, but fills in with the longer list rather than trimming to the
--shorter
zipSyls :: LineCons -> LineCons -> LineCons
zipSyls [] [] = []
zipSyls [] yl = yl
zipSyls xl [] = xl
zipSyls (x : xs) (y : ys) = mergeSyls x y : zipSyls xs ys

addConToSyl :: SylLoc -> PoemCons -> (Constraint, StateMod) -> PoemCons
addConToSyl sylLoc@(stanzaI, lineI, sylI) p c
  | badSylLoc sylLoc p = error $ "addConToSyl badSylLoc: " ++ show sylLoc
  | otherwise = modifiedPoem
  where
    modifiedSyl = mergeCons (p !! stanzaI !! lineI !! sylI) c
    modifiedLine = replace sylI modifiedSyl (p !! stanzaI !! lineI)
    modifiedStanza = replace lineI modifiedLine (p !! stanzaI)
    modifiedPoem = replace stanzaI modifiedStanza p

mergeCons :: SylCons -> (Constraint, StateMod) -> SylCons
mergeCons (cs, ms) (c, m) = (c : cs, m : ms)

mergeSyls :: SylCons -> SylCons -> SylCons
mergeSyls (c1s, m1s) (c2s, m2s) = (c1s ++ c2s, m1s ++ m2s)

replace :: Int -> a -> [a] -> [a]
replace i x xs =
  case splitAt i xs of
    (before, []) -> before ++ [x]
    (before, _ : after) -> before ++ [x] ++ after

badSylLoc :: SylLoc -> PoemCons -> Bool
badSylLoc (stanzaI, lineI, sylI) p
  | stanzaI >= length p = True
  | lineI >= length (p !! stanzaI) = True
  | sylI >= length (p !! stanzaI !! lineI) = True
  | otherwise = False
