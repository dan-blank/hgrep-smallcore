module FileSystem where

import System.Directory

{-|
  Takes a list of file names and returns
  a list of the files' contents.
-}
readFiles :: [String] -> IO [String]
readFiles [] = return []
readFiles (f : fs) = do
    exists <- doesFileExist f
    if exists then do
      this <- readFile f
      others <- readFiles fs
      return (this : others)
    else do
      putStrLn (f ++ ": File does not exist.")
      readFiles fs
