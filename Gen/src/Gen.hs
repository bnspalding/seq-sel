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
  )
where

import qualified Data.Text as T
import Debug.Trace
import Dictionary
import Gen.Constraints
import Sound

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
           in trace (printPoemCons (specConstraints spec)) $ _poem newSpec seqF [[[head es]]]

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
    then stanzas
    else trace (show (writePoem stanzas)) $ _poem newSpec seqF newStanzas
  where
    currentW = head $ wordsUsed spec
    nextTerm = head $ filter (`checkTerm` spec) (seqF spec currentW)
    (newSpec, isLineBreak, isStanzaBreak) = applyTerm nextTerm spec
    currentStanza = last stanzas
    currentLine = last currentStanza
    newStanzas =
      if isLineBreak
        then
          if isStanzaBreak
            then stanzas ++ [[[nextTerm]]]
            else init stanzas ++ [currentStanza ++ [[nextTerm]]]
        else init stanzas ++ [init currentStanza ++ [currentLine ++ [nextTerm]]]
    isComplete = null . specConstraints

-- | writePoem takes a generated poem and outputs it to text.
writePoem :: [Stanza] -> T.Text
writePoem = joinStanzas
  where
    joinStanzas s = T.intercalate "\n\n" $ joinLines <$> s
    joinLines ls = T.unlines $ joinTerms <$> ls
    joinTerms ts = T.unwords $ text <$> ts

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
checkTerm :: Term -> Spec -> Bool
checkTerm t spec
  | null sCons = error "The spec constraints are empty!"
  | null currentLine = error "The current line is zero length!"
  | length sylsT > length currentLine = False -- too long for current line
  | t `elem` wordsUsed spec = False -- enforce no repeats
  | otherwise = trace ("checking " ++ show (text t)) $ checkSyls spec sylsT currentCons
  where
    sylsT = pronunciation t
    sCons = specConstraints spec
    currentLine = head (head sCons)
    currentCons = fst <$> take (length sylsT) currentLine

checkSyls :: Spec -> [Syl] -> [[Constraint]] -> Bool
checkSyls _ [] _ = error "empty syl list in checkSyls!"
checkSyls _ _ [] = True
checkSyls spec [s] [cs] = and $ (\c -> c s spec) <$> cs -- this can be more clean
checkSyls spec (s : ss) (cs : css) = checkSyls spec [s] [cs] && checkSyls spec ss css

-- update the spec with the new term
applyTerm :: Term -> Spec -> (Spec, Bool, Bool)
applyTerm t spec =
  trace
    ("applying " ++ show (text t))
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
          if trace "--- line break ---" isStanzaBreak
            then
              if trace "--- stanza break ---" null remainingStanzas
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
