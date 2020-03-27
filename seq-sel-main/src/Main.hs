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
      output = poem spec seqFunc (T.pack word)
  TIO.putStrLn $ writePoem output
  TIO.putStrLn $ writeProns output

-- Select the Sequence Function from a given string ------
getSeqFunc :: String -> Seq
getSeqFunc "dict" = \spec e -> dropWhile (/= e) $ cycle $ toList $ dict spec
getSeqFunc "vec" = undefined
getSeqFunc _ = error "unknown sequence function"

fromOpts :: Opts -> IO Spec
fromOpts opts = do
  --d <- readDictFile $ optDictFile opts
  d <- readDictFile
  dFiltered <- filterDict d
  let lc = optLines opts
      r = T.pack $ optRhyme opts
      m = T.pack $ optMeter opts
      t = optRhymeThreshold opts
      c = T.pack $ optCustomConstraints opts
  trace ("filtered dictionary size is: " ++ show (size dFiltered)) (return ())
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
  return . filterDefs . filterText . subXPOS . flip subXTags filterTags $ d

filterPOSs :: [T.Text]
filterPOSs = ["name", "prefix", "suffix", "phrase"]

subXPOS :: Dictionary -> Dictionary
subXPOS = flip subDict $ \e ->
  not $
    all (\s -> Dictionary.pos s `elem` filterPOSs) (definitions e)
