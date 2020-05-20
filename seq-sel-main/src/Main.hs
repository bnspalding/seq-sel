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
import DictionaryIO
import Gen
import Options
import qualified Sequence.Dict as Dict
import qualified Sequence.Vec as Vec
import System.Environment
import Wiktionary (makeDictionary, readJSONL)

-- Main Execution ---------------------------------
main :: IO ()
main = do
  i <- execParser optsParser
  case i of
    (Input word optType) ->
      case optType of
        FromFile filename -> run word =<< readConfig filename
        FromFlags opts -> run word opts
    GenerateDict -> generateDictionary

run :: String -> Opts -> IO ()
run word opts = do
  spec <- fromOpts opts
  let seqFunc = getSeqFunc (optFunc opts)
      output = poem spec seqFunc (T.pack word)
  TIO.putStrLn "Text:"
  TIO.putStrLn $ writePoem output
  TIO.putStrLn "\nPronunciation:"
  TIO.putStrLn $ writeProns output
  TIO.putStrLn "\nStress:"
  TIO.putStrLn $ writeStress output

-- Select the Sequence Function from a given string ------
getSeqFunc :: String -> Seq
getSeqFunc "dict" = Dict.seqFunc
getSeqFunc "vec" = Vec.seqFunc
getSeqFunc _ = error "unknown sequence function"

fromOpts :: Opts -> IO Spec
fromOpts opts = do
  d <-
    if optDictVar opts == "local"
      then readDictionary "./data/localDict.jsonl"
      else
        readWiktVar (optDictVar opts)
          >>= filterDict (optFilterVar opts)
          >>= subList
  let lc = optLines opts
      r = T.pack $ optRhyme opts
      m = T.pack $ optMeter opts
      t = optRhymeThreshold opts
      c = T.pack $ optCustomConstraints opts
  trace ("filtered dictionary size is: " ++ show (size d)) (return ())
  return $ makeSpec lc r m d t c

-- TODO: the re-opened local dictionary appears to be smaller than when written
-- Check for what if falling through the cracks and fix it

generateDictionary :: IO ()
generateDictionary = do
  d <- readWiktVar "WIKTDATA_UTF8"
  dFiltered <- filterDict "FILTERWORDS" d >>= subList
  trace ("filtered dictionary size is: " ++ show (size dFiltered)) (return ())
  trace "writing dictionary to data/localDict.jsonl" (return ())
  writeDictionary "data/localDict.jsonl" dFiltered

readWiktFile :: String -> IO Dictionary
readWiktFile f = do
  wiktdata <- B.readFile f -- expecting .jsonl file here
  return $ makeDictionary $ rights $ readJSONL wiktdata

readWiktVar :: String -> IO Dictionary
readWiktVar dictVar = getEnv dictVar >>= readWiktFile

filterDict :: String -> Dictionary -> IO Dictionary
filterDict filterVar d = do
  let filterTags =
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
  if filterVar == ""
    then return . subXPOS . flip subXTags filterTags $ d
    else do
      filterFile <- getEnv filterVar
      filterList <- T.lines <$> TIO.readFile filterFile
      let filterDefs =
            flip
              subDict
              ( any
                  (null . flip intersect filterList . T.words . gloss)
                  . Set.toList
                  . definitions
              )
          filterText = flip subDict (not . (`elem` filterList) . text)
      return . filterDefs . filterText . subXPOS . flip subXTags filterTags $ d

filterPOSs :: [T.Text]
filterPOSs = ["name", "infix", "prefix", "suffix", "phrase"]

subXPOS :: Dictionary -> Dictionary
subXPOS = flip subDict $ \e ->
  not $
    all (\s -> Dictionary.pos s `elem` filterPOSs) (definitions e)

subList :: Dictionary -> IO Dictionary
subList d = do
  wordListFile <- getEnv "MYLEX"
  wordList <- T.lines <$> TIO.readFile wordListFile
  return $ subDict d (\e -> Dictionary.text e `elem` wordList)
