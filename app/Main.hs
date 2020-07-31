module Main where

import RegexAPI
import FileSystem
import Data.Maybe
import Data.Char
import Options.Applicative
import Data.Semigroup ((<>))

----------------------------------------------
-- Main
----------------------------------------------

main = 
  do
    cliargs <- execParser $ info (opt <**> helper)
          ( fullDesc
           <> progDesc "Search for lines in FILES by giving a search regex PATTERN"
           <> header "A primitive re-implentation of Grep using Haskell and Parser Derivates" )
    let flags = oflags cliargs
    let charmanipulator = if ignoreCase flags then toLower else id
    let dwe = fromJust $ parseRegex $ opattern cliargs
    contents <- readFiles $ ofiles cliargs -- list of contents of each existing file
    let manyLines = concatMap lines contents
    let doInvertFlag = invertMatch flags
    let highlightedLines = highlightLines dwe (map (map charmanipulator) manyLines) doInvertFlag
    mapM_ putStrLn highlightedLines

----------------------------------------------
-- Optparse
----------------------------------------------

data Opt = Opt
  {
    opattern :: String,
    ofiles :: [String],
    oflags :: Flags
  }

opt :: Parser Opt
opt = Opt
    <$> argument str (metavar "PATTERN")
    <*> some (argument str (metavar "FILES..."))
    <*> pFlags

data Flags = Flags
  { invertMatch :: Bool
  , ignoreCase :: Bool }

pFlags :: Parser Flags
pFlags = Flags
    <$> switch 
        ( long "invert-match"
        <> help "Highlight everything that is not matched." )
    <*> switch
        ( long "ignore-case"
        <> help "Don't distinguisch between lower and upercase letters." )