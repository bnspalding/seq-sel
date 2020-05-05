{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Gen
-- Description: poem generation
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: CC-BY-NC
-- Stability: experimental
--
-- Gen is the underlying engine that generates _Sequence, Selection_ poems. It
-- uses syllable level constraints (see "Gen.Constraints") to select terms from
-- a sequence. A poem is structured as lists of terms organized into lines and
-- stanzas. These are then rendered out to text using 'writePoem'.
module Gen
  ( -- * Types
    Seq,
    Term,
    Line,
    Stanza,

    -- * Specifications
    Spec (..),
    makeSpec,

    -- * Generation
    poem,

    -- * Output
    writePoem,
    writeProns,
    writeStress,
  )
where

import Data.List (find)
import Data.Maybe
import qualified Data.Text as T
import Dictionary
import Selection.Constraints
import Sound
import Sound.Syl

-- | A Term is a word and its surrounding information, or, in other words, a
-- dictionary Entry
type Term = Entry

-- | A Line (as in a line of the output poem) is simply a list of Terms
type Line = [Term]

-- | A Stanza (as in a stanza of the output poem) is a list (grouping) of Lines
type Stanza = [Line]

-- | Seq describes a function that generates a sequence of Terms from a Term,
-- optionally using the spec to generate the list (to be selected from according
-- to constraints during generation)
type Seq = (Spec -> Term -> [Term])

-- | poem is the primary generation function. Given a specification, a sequence
-- function, and a starting word, it generates a poem from the sequence
-- satisfying the constraints of the specification.
poem :: Spec -> Seq -> T.Text -> [Stanza]
poem spec seqF firstWord =
  let d = dict spec
      esMaybe = lookupText d firstWord
   in case esMaybe of
        Nothing -> error "the given word is not present in the dictionary"
        Just es ->
          let (newSpec, _, _) = applyTerm (head es) spec
           in _poem newSpec seqF [[[head es]]]

_poem :: Spec -> Seq -> [Stanza] -> [Stanza]
-- First Word Case: some special handling is required for the first word
-- (such as filling with a previous word to get the meter right)
-- also, need to verify that the given word does indeed exist in dict
-- TODO: add special handling (currently in poem above)
-- be careful to not hit an infinite loop on the first word
-- _poem spec seqF [[[w]]] = undefined
-- Standard Case: add words to the poem until the spec is fully realized
_poem spec seqF stanzas =
  if isComplete spec
    then filter (/= [[]]) stanzas -- filter out empty stanzas; inelegant but simple
    else _poem newSpec seqF newStanzas
  where
    currentW = head $ wordsUsed spec
    termList = seqF spec currentW
    highRigorList = find (\t -> checkTerm t spec High) $ take 2000 termList
    medRigorList = find (\t -> checkTerm t spec Medium) $ take 8000 termList
    lowRigorList = find (\t -> checkTerm t spec Low) $ take 32000 termList
    noRigorList = find (\t -> checkTerm t spec None) termList
    nextTerm =
      head $ catMaybes [highRigorList, medRigorList, lowRigorList, noRigorList]
    (newSpec, isLineBreak, isStanzaBreak) = applyTerm nextTerm spec
    currentStanza = last stanzas
    currentLine = last currentStanza
    newCL = currentLine ++ [nextTerm]
    newCS = init currentStanza ++ [newCL]
    emptyLine = []
    emptyStanza = [emptyLine]
    newStanzas =
      -- lineBreaks and stanza breaks are for the next word, not the word
      -- currently being placed
      if isLineBreak
        then
          if isStanzaBreak
            then init stanzas ++ [newCS] ++ [emptyStanza]
            else init stanzas ++ [newCS ++ [emptyLine]]
        else init stanzas ++ [newCS]
    isComplete = null . specConstraints

-- | writePoem takes a generated poem and outputs it to text.
writePoem :: [Stanza] -> T.Text
writePoem = _outPoem text

-- | writeProns takes a generated poem and outputs the pronunciations of its
-- words
writeProns :: [Stanza] -> T.Text
writeProns = _outPoem pronToText
  where
    pronToText t = T.intercalate "." (sylToText <$> pronunciation t)
    sylToText s = T.concat $ getSymbol <$> sounds s
    getSymbol (Sound s) = s

-- | writeStress takes a generated poem and outputs the stress patterns of its
-- words
writeStress :: [Stanza] -> T.Text
writeStress = _outPoem stressToText
  where
    stressToText t = T.concat ["(", T.intercalate "," (printStress <$> pronunciation t), ")"]
    printStress syl =
      case stress syl of
        Stressed -> "1"
        SecondaryStress -> "2"
        Unstressed -> "0"
        ReducedStress -> "r"
        NullStress -> "*"

_outPoem :: (Term -> T.Text) -> [Stanza] -> T.Text
_outPoem f = T.intercalate "\n" . fmap (T.unlines . fmap (T.unwords . fmap f))

-- makeSpec generates a Spec from its different parts:
--
-- lineCount rhymeScheme meterScheme dictionary rhymeThreshold customConstraints
makeSpec :: Int -> T.Text -> T.Text -> Dictionary -> Float -> T.Text -> Spec
makeSpec lineCount rhymeS meterS d rThreshold customCons =
  let cs = makeCons lineCount meterS rhymeS rThreshold customCons
   in Spec
        { specConstraints = cs,
          wordsUsed = [],
          rhymeMap = makeRhymeMap rhymeS,
          dict = d
        }

-- Evaluating Terms with the Spec

-- check if a term satisfies all the current front constraints
checkTerm :: Term -> Spec -> RigorLevel -> Bool
checkTerm t spec rl
  | null sCons = error "The spec constraints are empty!"
  | null currentLine = error "The current line is zero length!"
  | length sylsT > length currentLine = False -- too long for current line
  | t `elem` wordsUsed spec = False -- enforce no repeats
  | otherwise = checkSyls spec rl sylsT currentCons
  where
    sylsT = pronunciation t
    sCons = specConstraints spec
    currentLine = head (head sCons)
    currentCons = fst <$> take (length sylsT) currentLine

checkSyls :: Spec -> RigorLevel -> [Syl] -> [[Constraint]] -> Bool
checkSyls _ _ [] _ = error "empty syl list in checkSyls!"
checkSyls _ _ _ [] = True
checkSyls spec rl [s] [cs] = and $ (\c -> c rl s spec) <$> cs -- this can be more clean
checkSyls spec rl (s : ss) (cs : css) = checkSyls spec rl [s] [cs] && checkSyls spec rl ss css

-- update the spec with the new term
applyTerm :: Term -> Spec -> (Spec, Bool, Bool)
applyTerm t spec =
  ( applySpecs
      currentMods
      sylsT
      Spec
        { specConstraints = trimmedCons,
          wordsUsed = t : wordsUsed spec,
          rhymeMap = rhymeMap spec,
          dict = dict spec
        },
    isLineBreak,
    isStanzaBreak
  )
  where
    sylsT = pronunciation t
    sCons = specConstraints spec
    currentLine = head (head sCons)
    currentMods = snd <$> take (length sylsT) currentLine
    newCurrentLine = drop (length sylsT) currentLine
    remainingLines = tail (head sCons)
    remainingStanzas = tail sCons
    isLineBreak = null newCurrentLine
    isStanzaBreak = null remainingLines
    trimmedCons =
      -- clear out empty lists when trimming
      if isLineBreak
        then
          if isStanzaBreak
            then
              if null remainingStanzas
                then []
                else remainingStanzas
            else remainingLines : remainingStanzas
        else (newCurrentLine : remainingLines) : remainingStanzas

_applySpecs :: [SpecMod] -> Syl -> Spec -> Spec
_applySpecs (m : ms) s spec = _applySpecs ms s (m spec s)
_applySpecs [] _ spec = spec

applySpecs :: [[SpecMod]] -> [Syl] -> Spec -> Spec
applySpecs (ms : mss) (s : ss) spec = applySpecs mss ss (_applySpecs ms s spec)
applySpecs [] _ spec = spec
applySpecs _ [] _ = error "empty syl set in applySpecs"
