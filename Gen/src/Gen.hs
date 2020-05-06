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
import Poem (Line, Poem, Stanza, Term)
import Selection.Constraints
import Sequence (Seq)
import Sound
import Sound.Syl
import Spec

-- | poem is the primary generation function. Given a specification, a sequence
-- function, and a starting word, it generates a poem from the sequence
-- satisfying the constraints of the specification.
poem :: Spec -> Seq -> T.Text -> Poem
poem spec seqF firstWord =
  let res = resources spec
      poemState = state spec
      d = dict res
      esMaybe = lookupText d firstWord
   in case esMaybe of
        Nothing -> error "the given word is not present in the dictionary"
        Just es ->
          let (newState, _, _) = applyTerm (head es) poemState
           in _poem (Spec newState res) seqF [[[head es]]]

_poem :: Spec -> Seq -> [Stanza] -> Poem
-- First Word Case: some special handling is required for the first word
-- (such as filling with a previous word to get the meter right)
-- also, need to verify that the given word does indeed exist in dict
-- TODO: add special handling (currently in poem above)
-- be careful to not hit an infinite loop on the first word
-- _poem spec seqF [[[w]]] = undefined
-- Standard Case: add words to the poem until the spec is fully realized
_poem spec seqF stanzas =
  if isComplete poemState
    then filter (/= [[]]) stanzas -- filter out empty stanzas; inelegant but simple
    else _poem (Spec newState res) seqF newStanzas
  where
    poemState = state spec
    res = resources spec
    currentW = head $ wordsUsed poemState
    termList = seqF spec currentW
    highRigorList = find (\t -> checkTerm t poemState High) $ take 2000 termList
    medRigorList = find (\t -> checkTerm t poemState Medium) $ take 8000 termList
    lowRigorList = find (\t -> checkTerm t poemState Low) $ take 32000 termList
    noRigorList = find (\t -> checkTerm t poemState None) termList
    nextTerm =
      head $ catMaybes [highRigorList, medRigorList, lowRigorList, noRigorList]
    (newState, isLineBreak, isStanzaBreak) = applyTerm nextTerm poemState
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
writePoem :: Poem -> T.Text
writePoem = _outPoem text

-- | writeProns takes a generated poem and outputs the pronunciations of its
-- words
writeProns :: Poem -> T.Text
writeProns = _outPoem pronToText
  where
    pronToText t = T.intercalate "." (sylToText <$> pronunciation t)
    sylToText s = T.concat $ getSymbol <$> sounds s
    getSymbol (Sound s) = s

-- | writeStress takes a generated poem and outputs the stress patterns of its
-- words
writeStress :: Poem -> T.Text
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

_outPoem :: (Term -> T.Text) -> Poem -> T.Text
_outPoem f = T.intercalate "\n" . fmap (T.unlines . fmap (T.unwords . fmap f))

-- makeSpec generates a Spec from its different parts:
--
-- lineCount rhymeScheme meterScheme dictionary rhymeThreshold customConstraints
makeSpec :: Int -> T.Text -> T.Text -> Dictionary -> Float -> T.Text -> Spec
makeSpec lineCount rhymeS meterS d rThreshold customCons =
  let cs = makeCons lineCount meterS rhymeS rThreshold customCons
   in Spec
        PoemState
          { specConstraints = cs,
            wordsUsed = [],
            rhymeMap = makeRhymeMap rhymeS
          }
        Resources {dict = d}

-- Evaluating Terms with the Spec

-- check if a term satisfies all the current front constraints
checkTerm :: Term -> PoemState -> RigorLevel -> Bool
checkTerm t poemState rl
  | null sCons = error "The spec constraints are empty!"
  | null currentLine = error "The current line is zero length!"
  | length sylsT > length currentLine = False -- too long for current line
  | t `elem` wordsUsed poemState = False -- enforce no repeats
  | otherwise = checkSyls poemState rl sylsT currentCons
  where
    sylsT = pronunciation t
    sCons = specConstraints poemState
    currentLine = head (head sCons)
    currentCons = fst <$> take (length sylsT) currentLine

checkSyls :: PoemState -> RigorLevel -> [Syl] -> [[Constraint]] -> Bool
checkSyls _ _ [] _ = error "empty syl list in checkSyls!"
checkSyls _ _ _ [] = True
checkSyls spec rl [s] [cs] = and $ (\c -> c rl s spec) <$> cs -- this can be more clean
checkSyls spec rl (s : ss) (cs : css) = checkSyls spec rl [s] [cs] && checkSyls spec rl ss css

-- update the spec with the new term
applyTerm :: Term -> PoemState -> (PoemState, Bool, Bool)
applyTerm t poemState =
  ( applySpecs
      currentMods
      sylsT
      PoemState
        { specConstraints = trimmedCons,
          wordsUsed = t : wordsUsed poemState,
          rhymeMap = rhymeMap poemState
        },
    isLineBreak,
    isStanzaBreak
  )
  where
    sylsT = pronunciation t
    sCons = specConstraints poemState
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

_applySpecs :: [StateMod] -> Syl -> PoemState -> PoemState
_applySpecs (m : ms) s spec = _applySpecs ms s (m spec s)
_applySpecs [] _ spec = spec

applySpecs :: [[StateMod]] -> [Syl] -> PoemState -> PoemState
applySpecs (ms : mss) (s : ss) spec = applySpecs mss ss (_applySpecs ms s spec)
applySpecs [] _ spec = spec
applySpecs _ [] _ = error "empty syl set in applySpecs"
