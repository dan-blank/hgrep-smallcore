{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Constants where

import Data.Range.Range as R
import GHC.Generics (Generic)
import Data.Hashable
import Data.Maybe
import Data.Char
import Data.List

newtype CRange = CRange { ranges :: [R.Range Int] } deriving (Eq, Ord, Generic)

deriving instance Generic (Range Int)
deriving instance Ord (Range Int)

instance Hashable (Range Int)
instance Hashable CRange



type Encoding = [[Range Int]]




---------------------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------------------



empty       = -1 :: Int
anchorBeg       = -2 :: Int
anchorEnd       = -3:: Int
highestUnicode = 136755 :: Int

getRepresentative :: [Range Int] -> Int
getRepresentative rs = (head $ fromRanges $ R.invert rs) - 1

mapToRepresentative :: Encoding -> Int -> Int
mapToRepresentative enc i = getRepresentative $ fromJust $ find (\r -> R.inRanges r i) enc

mapRToRepresentative :: Encoding -> [Range Int] -> Int
mapRToRepresentative enc r = mapToRepresentative enc (getRepresentative r)





-- Non-isomorphic printing
intToChar :: Int -> String
intToChar 136756 = "Top"
intToChar 0 = "NULL"
intToChar (-1) = "ε"
intToChar (-2) = "^"
intToChar (-3) = "$"
intToChar (-4) = "Bottom"
intToChar c = [chr c]

alphabet :: Range Int
alphabet = R.SpanRange anchorEnd highestUnicode