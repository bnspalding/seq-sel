{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as B
import Data.Either (rights)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
getSeqFunc "dict" = undefined
getSeqFunc "vec" = undefined
getSeqFunc _ = error "unknown sequence function"

fromOpts :: Opts -> IO Spec
fromOpts opts = do
  --d <- readDictFile $ optDictFile opts
  d <- readDictFile
  let lc = optLines opts
      r = T.pack $ optRhyme opts
      m = T.pack $ optMeter opts
      t = optRhymeThreshold opts
      c = T.pack $ optCustomConstraints opts
  return $ makeSpec lc r m d t c

-- TODO: this should take an option (see fromOpts)
readDictFile :: IO Dictionary
readDictFile = do
  f <- getEnv "WIKTDATA_UTF8"
  wiktdata <- B.readFile f -- expecting .jsonl file here
  return $ makeDictionary $ rights $ readJSONL wiktdata
