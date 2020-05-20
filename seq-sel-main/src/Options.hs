{-# LANGUAGE OverloadedStrings #-}

module Options
  ( Opts (..),
    OptionType (..),
    Input (..),
    optsParser,
    execParser, -- from Options.Applicative
    readConfig,
  )
where

import Data.Semigroup ((<>))
import qualified Data.Yaml as Y
import Options.Applicative

data Opts
  = Opts
      { optFunc :: !String,
        optLines :: !Int,
        optRhyme :: !String,
        optMeter :: !String,
        optDictVar :: !String,
        optFilterVar :: !String,
        optRhymeThreshold :: !Float,
        optCustomConstraints :: !String
      }
  deriving (Show)

data OptionType
  = FromFile FilePath
  | FromFlags Opts

data Input
  = Input String OptionType
  | GenerateDict

-- Flag Parser info -----------------------------------
optsParser :: ParserInfo Input
optsParser =
  info
    (helper <*> versionOption <*> (parseInput <|> parseGenDict))
    (fullDesc <> header "Sequence, Selection - program description")

versionOption :: Parser (a -> a)
versionOption = infoOption "0.0" (long "version" <> help "Show version")

parseGenDict :: Parser Input
parseGenDict = flag' GenerateDict (long "generate-dict")

parseInput :: Parser Input
parseInput = Input <$> parseWord <*> (parseFile <|> parseFlags)

parseWord :: Parser String
parseWord =
  argument
    str
    (metavar "WORD" <> help "provide an input word for poetry generation")

parseFile :: Parser OptionType
parseFile =
  FromFile
    <$> strOption
      ( long "config"
          <> short 'c'
          <> metavar "FILENAME"
          <> help "specify options as a .yaml config file"
      )

parseFlags :: Parser OptionType
parseFlags = FromFlags <$> parseOpts

parseOpts :: Parser Opts
parseOpts =
  Opts
    <$> strOption
      ( long "func"
          <> short 'f'
          <> metavar "FUNC"
          <> help "specify the sequence function"
      )
    <*> option
      auto
      ( long "lines"
          <> short 'l'
          <> metavar "LINES"
          <> value 4
          <> help "specify the number of lines to generate"
      )
    <*> strOption
      ( long "rhyme"
          <> short 'r'
          <> metavar "SCHEME"
          <> value "abab"
          <> help "specify the rhyme scheme"
      )
    <*> strOption
      ( long "meter"
          <> short 'm'
          <> metavar "SCHEME"
          <> value "01010101/010101"
          <> help "specify the meter"
      )
    <*> strOption
      ( long "dictVar"
          <> short 'd'
          <> metavar "DICTIONARY_VAR"
          <> help "environment variable pointing to file from which dictionary and pronunciation are constructed"
      )
    <*> strOption
      ( long "filterVar"
          <> short 'x'
          <> metavar "FILTER_VAR"
          <> value ""
          <> help "environment variable pointing to .words file from which the dictionary is filtered"
      )
    <*> option
      auto
      ( long "rhymeThreshold"
          <> short 't'
          <> value 1.0
          <> help "similarity value used in rhyming (between 0 and 1)."
      )
    <*> strOption
      ( long "customCons"
          <> short 'c'
          <> metavar "CONSTRAINT_STRING"
          <> value ""
          <> help "provide a custom syllable-level constraint set"
      )

-- Read config.yaml file ---------------------------------
instance Y.FromJSON Opts where
  parseJSON (Y.Object m) =
    Opts <$> m Y..: "func" <*> m Y..: "lines" <*> m Y..: "rhyme"
      <*> m Y..: "meter"
      <*> m Y..: "dictVar"
      <*> m Y..: "filterVar"
      <*> m Y..: "rhymeThreshold"
      <*> m Y..: "customCons"
  parseJSON _ = fail "Expected Object for Config value"

readConfig :: String -> IO Opts
readConfig filename = either (error . show) id <$> Y.decodeFileEither filename
