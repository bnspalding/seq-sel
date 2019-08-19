module Main where

import Data.Semigroup ((<>))
import Options.Applicative

data Opts = Opts {
    optFunc :: !String,
    optLines :: !Int,
    optRhyme :: !String,
    optMeter :: !String,
    optConfig :: !String
}

main :: IO ()
main = do
    opts <- execParser optsParser
    putStrLn $ "sequence-selection" ++ optFunc opts
    where
        optsParser :: ParserInfo Opts
        optsParser = 
            info 
                (helper <*> versionOption <*> programOptions)
                (fullDesc <> progDesc "Sequence, Selection" <>
                header "Sequence, Selection - program description")
        versionOption :: Parser (a -> a)
        versionOption = infoOption "0.0" (long "version" <> help "Show version")
        programOptions :: Parser Opts
        programOptions = Opts 
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
            <*> strOption
                 ( long "config" 
                <> metavar "FILENAME.yaml"
                <> value "none" 
                <> help "specify options as a .yaml config file")
