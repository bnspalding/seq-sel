{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Semigroup ((<>))
import qualified Data.Yaml as Y
import Gen
import Options.Applicative

data Opts =
  Opts
    { optFunc :: !String
    , optLines :: !Int
    , optRhyme :: !String
    , optMeter :: !String
    , optPronFile :: !String
    , optRhymeThreshold :: !Float
    , optCustomConstraints :: !String
    }
  deriving (Show)

data OptionType
  = FromFile FilePath
  | FromFlags Opts

data Input =
  Input String OptionType

-- Main Execution ---------------------------------
main :: IO ()
main = do
  (Input word optType) <- execParser optsParser
  case optType of
    FromFile filename -> run word =<< readConfig filename
    FromFlags opts -> run word opts

run :: String -> Opts -> IO ()
run word opts =
  putStrLn $
  writePoem $
  poem (makeSpec opts) (getSeqFunc (optFunc opts)) (prepFirstWord word)

-- Flag Parser info -----------------------------------        
optsParser :: ParserInfo Input
optsParser =
  info
    (helper <*> versionOption <*> parseInput)
    (fullDesc <> header "Sequence, Selection - program description")

versionOption :: Parser (a -> a)
versionOption = infoOption "0.0" (long "version" <> help "Show version")

parseInput :: Parser Input
parseInput = Input <$> parseWord <*> (parseFile <|> parseFlags)

parseWord :: Parser String
parseWord =
  argument
    str
    (metavar "WORD" <> help "provide an input word for poetry generation")

parseFile :: Parser OptionType
parseFile =
  FromFile <$>
  strOption
    (long "config" <>
     short 'c' <>
     metavar "FILENAME" <> help "specify options as a .yaml config file")

parseFlags :: Parser OptionType
parseFlags = FromFlags <$> parseOpts

parseOpts :: Parser Opts
parseOpts =
  Opts <$>
  strOption
    (long "func" <>
     short 'f' <> metavar "FUNC" <> help "specify the sequence function") <*>
  option
    auto
    (long "lines" <>
     short 'l' <>
     metavar "LINES" <>
     value 4 <> help "specify the number of lines to generate") <*>
  strOption
    (long "rhyme" <>
     short 'r' <>
     metavar "SCHEME" <> value "abab" <> help "specify the rhyme scheme") <*>
  strOption
    (long "meter" <>
     short 'm' <>
     metavar "SCHEME" <> value "01010101/010101" <> help "specify the meter") <*>
  strOption
    (long "pronFile" <>
     short 'p' <>
     metavar "PRONUNCIATION_FILE" <>
     help "file from which dictionary and pronunciation info is constructed") <*>
  option
    auto
    (long "rhymeThreshold" <>
     short 't' <>
     value 1.0 <> help "similarity value used in rhyming (between 0 and 1).") <*>
  strOption
    (long "customCons" <>
     short 'c' <>
     metavar "CONSTRAINT_STRING" <>
     value "" <> help "provide a custom syllable-level constraint set")

-- Read config.yaml file ---------------------------------
instance Y.FromJSON Opts where
  parseJSON (Y.Object m) =
    Opts <$> m Y..: "func" <*> m Y..: "lines" <*> m Y..: "rhyme" <*>
    m Y..: "meter" <*>
    m Y..: "pronFile" <*>
    m Y..: "rhymeThreshold" <*>
    m Y..: "customCons"
  parseJSON _ = fail "Expected Object for Config value"

readConfig :: String -> IO Opts
readConfig filename = either (error . show) id <$> Y.decodeFileEither filename

-- Select the Sequence Function from a given string ------
getSeqFunc :: String -> Seq
getSeqFunc "dict" = undefined
getSeqFunc "vec" = undefined
getSeqFunc _ = error "unknown sequence function"

makeSpec :: Opts -> Spec
makeSpec opts = undefined

-- TODO: Provide special handling for the first word here (such as filling with
-- a previous word to get the meter right). This will require a hook into the
-- dictionary and most likely Gen
prepFirstWord :: String -> [Stanza]
prepFirstWord word = [[[getTerm word]]]

-- use this as an opportunity to check that the first word exists in the
-- dictionary. Probably go to a Maybe val and back, with an error on Nothing.
getTerm :: String -> Term
getTerm = undefined
