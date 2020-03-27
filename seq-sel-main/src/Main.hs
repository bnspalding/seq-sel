{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as B
import Data.Either (rights)
import Data.List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import Dictionary
import Gen
import OptionsParsing
import System.Environment
import Wiktionary (makeDictionary, readJSONL)

-- Main Execution ---------------------------------
main :: IO ()
main = do
  (Input word optType) <- execParser optsParser
  case optType of
    FromFile filename -> run word =<< readConfig filename
    FromFlags opts -> run word opts

run :: String -> Opts -> IO ()
run word opts = do
  spec <- fromOpts opts
  let seqFunc = getSeqFunc (optFunc opts)
  TIO.putStrLn $ writePoem $ poem spec seqFunc (T.pack word)

-- Select the Sequence Function from a given string ------
getSeqFunc :: String -> Seq
getSeqFunc "dict" = \spec e -> dropWhile (/= e) $ cycle $ toList $ dict spec
getSeqFunc "vec" = undefined
getSeqFunc _ = error "unknown sequence function"

fromOpts :: Opts -> IO Spec
fromOpts opts = do
  --d <- readDictFile $ optDictFile opts
  d <- readDictFile
  dFiltered <- trace "dict filtered" <$> filterDict d
  let lc = optLines opts
      r = T.pack $ optRhyme opts
      m = T.pack $ optMeter opts
      t = optRhymeThreshold opts
      c = T.pack $ optCustomConstraints opts
  return $ makeSpec lc r m dFiltered t c

-- TODO: this should take an option (see fromOpts)
readDictFile :: IO Dictionary
readDictFile = do
  f <- getEnv "WIKTDATA_UTF8"
  wiktdata <- B.readFile f -- expecting .jsonl file here
  return $ makeDictionary $ rights $ readJSONL wiktdata

filterDict :: Dictionary -> IO Dictionary
filterDict d = do
  filterFile <- getEnv "FILTERWORDS"
  filterList <- T.lines <$> TIO.readFile filterFile
  let filterDefs =
        flip
          subDict
          ( any
              (null . flip intersect filterList . T.words . gloss)
              . Set.toList
              . definitions
          )
      filterTags =
        [ "offensive",
          "derogatory",
          "informal",
          "spoken",
          "imitating Irish accent",
          "Singlish",
          "Braille",
          "humorous",
          "vulgar",
          "colloquial",
          "ethnic slur",
          "religious slur",
          "informal",
          "slang",
          "archaic",
          "rare",
          "obsolete",
          "Singapore"
        ]
      filterText = flip subDict (not . (`elem` filterList) . text)
  return . filterDefs . filterText . flip subXTags filterTags $ d
