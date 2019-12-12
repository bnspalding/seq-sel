{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dictionary
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
run word opts = do
  spec <- fromOpts opts
  let seqFunc = getSeqFunc (optFunc opts)
  putStrLn $ writePoem $ poem spec seqFunc word

-- Select the Sequence Function from a given string ------
getSeqFunc :: String -> Seq
getSeqFunc "dict" = undefined
getSeqFunc "vec" = undefined
getSeqFunc _ = error "unknown sequence function"

fromOpts :: Opts -> IO Spec
fromOpts opts = do
  d <- readDictFile $ optDictFile opts
  let lc = optLines opts
      r = optRhyme opts
      m = optMeter opts
      t = optRhymeThreshold opts
      c = optCustomConstraints opts
  return $ makeSpec lc r m d t c

readDictFile :: String -> IO Dictionary
readDictFile dictFile = do
  ls <- lines <$> readFile dictFile
  return $ fromList (toEntry <$> ls)
  where
    toEntry s =
      let ws = words s
       in if length ws == 2
            then let (w, p) = (head ws, ws !! 1)
                  in makeEntry w [] [] p
            else error "unknown format in dictFile"
