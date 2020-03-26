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
    Spec,
    makeSpec,

    -- * Generation
    poem,

    -- * Output
    writePoem,
  )
where

import Data.List
import qualified Data.Set as Set
import qualified Data.Text as T
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
        Just es -> _poem spec seqF [[[head es]]]

_poem :: Spec -> Seq -> [Stanza] -> [Stanza]
-- First Word Case: some special handling is required for the first word
-- (such as filling with a previous word to get the meter right)
-- also, need to verify that the given word does indeed exist in dict
_poem spec seqF [[[w]]] = undefined
-- Standard Case: add words to the poem until the spec is fully realized
_poem spec seqF stanzas = undefined

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
      rm = makeRhymeMap rhymeS
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
  | otherwise = checkSyls spec sylsT currentCons
  where
    sylsT = pronunciation t
    sCons = specConstraints spec
    currentLine = head (head sCons)
    currentCons = fst <$> take (length sylsT) currentLine

checkSyls :: Spec -> [Syl] -> [[Constraint]] -> Bool
checkSyls _ [] _ = error "empty syl list in checkSyls!"
checkSyls spec [s] [cs] = and $ (\c -> c s spec) <$> cs -- this can be more clean
checkSyls spec (s : ss) (cs : css) = checkSyls spec [s] [cs] && checkSyls spec ss css

-- update the spec with the new term
applyTerm :: Term -> Spec -> Spec
applyTerm t spec =
  applySpecs
    currentMods
    sylsT
    Spec
      { specConstraints = trimmedCons,
        wordsUsed = t : wordsUsed spec,
        rhymeMap = rhymeMap spec,
        dict = dict spec
      }
  where
    sylsT = pronunciation t
    sCons = specConstraints spec
    currentLine = head (head sCons)
    currentMods = snd <$> take (length sylsT) currentLine
    trimmedCons = (drop (length sylsT) currentLine : tail (head sCons)) : tail sCons

_applySpecs :: [SpecMod] -> Syl -> Spec -> Spec
_applySpecs (m : ms) s spec = _applySpecs ms s (m spec s)
_applySpecs [] _ spec = spec

applySpecs :: [[SpecMod]] -> [Syl] -> Spec -> Spec
applySpecs (ms : mss) (s : ss) spec = applySpecs mss ss (_applySpecs ms s spec)
applySpecs [] _ spec = spec
