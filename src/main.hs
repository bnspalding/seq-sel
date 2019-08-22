{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import qualified Data.Yaml as Y

data Opts = Opts {
    optFunc :: !String,
    optLines :: !Int,
    optRhyme :: !String,
    optMeter :: !String
} deriving Show

data OptionType
    = FromFile   FilePath
    | FromFlags  Opts

data Input = Input String OptionType

-- Main Execution ---------------------------------

main :: IO ()
main = do 
    (Input word optType) <- execParser optsParser
    case optType of
        FromFile filename -> run word =<< readConfig filename
        FromFlags opts -> run word opts    

run :: String -> Opts -> IO ()
run word opts = putStrLn $ "sequence-selection on word: " ++ word
        ++ " func: " ++ optFunc opts
        -- TODO: replace this with a call to gen

-- Flag Parser info -----------------------------------        

optsParser :: ParserInfo Input
optsParser = info 
    (helper <*> versionOption <*> parseInput)
    (  fullDesc 
    <> header "Sequence, Selection - program description")

versionOption :: Parser (a -> a)
versionOption = infoOption "0.0" (long "version" <> help "Show version")

parseInput :: Parser Input
parseInput = Input <$> parseWord <*> (parseFile <|> parseFlags)

parseWord :: Parser String
parseWord = argument str
    ( metavar "WORD"
    <> help "provide an input word for poetry generation")

parseFile :: Parser OptionType
parseFile = FromFile
    <$> strOption
        ( long "config"
        <> short 'c' 
        <> metavar "FILENAME"
        <> help "specify options as a .yaml config file")

parseFlags :: Parser OptionType
parseFlags = FromFlags <$> parseOpts

parseOpts :: Parser Opts
parseOpts = Opts
    <$> strOption 
         ( long "func"
        <> short 'f'  
        <> metavar "FUNC" 
        <> help "specify the sequence function")
    <*> option auto
         ( long "lines" 
        <> short 'l' 
        <> metavar "LINES" 
        <> value 4 
        <> help "specify the number of lines to generate") 
    <*> strOption
         ( long "rhyme" 
        <> short 'r' 
        <> metavar "SCHEME" 
        <> value "abab" 
        <> help "specify the rhyme scheme")
    <*> strOption
         ( long "meter" 
        <> short 'm' 
        <> metavar "SCHEME" 
        <> value "01010101/010101" 
        <> help "specify the meter")

-- Read config.yaml file ---------------------------------

instance Y.FromJSON Opts where
    parseJSON (Y.Object m) = Opts <$>
        m Y..: "func" <*>
        m Y..: "lines" <*>
        m Y..: "rhyme" <*>
        m Y..: "meter"
    parseJSON _ = fail "Expected Object for Config value"

readConfig :: String -> IO Opts
readConfig filename = 
    either (error . show) id <$>
    Y.decodeFileEither filename
