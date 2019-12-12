{-# LANGUAGE OverloadedStrings #-}

module Main where

import Gen
import OptionsParsing

-- Main Execution ---------------------------------
main :: IO ()
main = do
  (Input word optType) <- execParser optsParser
  case optType of
    FromFile filename -> run word =<< readConfig filename
    FromFlags opts -> run word opts

run :: String -> Opts -> IO ()
run word opts =
  putStrLn $ writePoem $ poem (fromOpts opts) (getSeqFunc (optFunc opts)) word

-- Select the Sequence Function from a given string ------
getSeqFunc :: String -> Seq
getSeqFunc "dict" = undefined
getSeqFunc "vec" = undefined
getSeqFunc _ = error "unknown sequence function"

fromOpts :: Opts -> Spec
fromOpts opts = makeSpec lc r m d t c
  where
    lc = optLines opts
    r = optRhyme opts
    m = optMeter opts
    d = optDictFile opts
    t = optRhymeThreshold opts
    c = optCustomConstraints opts
