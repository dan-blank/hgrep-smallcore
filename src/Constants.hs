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

empty = -1 :: Int -- This represents the empty input.
anchorBeg = -2 :: Int -- The anchor that marks the beginning of a word.
anchorEnd = -3 :: Int -- The anchor that marks the end of a word.
highestUnicode = 136755 :: Int -- The highest possible input in Unicode.
alphabet = R.SpanRange anchorEnd highestUnicode :: Range Int -- The range of possible inputs.

-- A character range represents a character class of continous characters, e.g. [1], [123], [a-z].
-- Character sets, the central datastructure, is composed of CRanges.
newtype CRange = CRange { ranges :: [R.Range Int] } deriving (Eq, Ord, Generic)

deriving instance Generic (Range Int)
-- We need Ord in order to get unique canonical forms - unions of regexes should be ordered.
deriving instance Ord (Range Int)
-- We need Hashable since we will use Character sets as keys for calculating DFA state transitions.
instance Hashable (Range Int)
instance Hashable CRange

pattern RLU n m rs = (CRange (R.LowerBoundRange n : R.UpperBoundRange m : rs))
pattern RSpan n m rs = (CRange (R.SpanRange n m : rs))
pattern PEmpty = CRange [SingletonRange (-1)]

instance Show CRange where
    show (CRange [])                            = ""
    show (CRange (R.SingletonRange n : rs))     = intToChar n ++ show (CRange rs) 
    show (RSpan (-3) 136755 rs)                 = "Σ" ++ show (CRange rs)
    show (RLU 136756 (-4) rs)                   = "Ø" ++ show (CRange rs)
    show (RSpan l r rs)                         = "[" ++ intToChar l ++ "," ++ intToChar r ++ "]" ++ show (CRange rs)
    show (CRange (R.LowerBoundRange l : rs))    = "[" ++ intToChar l ++ ",∞]" ++ show (CRange rs)
    show (CRange (R.UpperBoundRange r : rs))    = "[∞," ++ intToChar r ++ "]" ++ show (CRange rs)
    show (CRange (R.InfiniteRange : rs))        = "∞" ++ show (CRange rs)

-- Every possible input character belongs to a class that represents it.
type Encoding = [[Range Int]]
-- Given a representation class, get the lowest member as its representant.
getRepresentative :: [Range Int] -> Int
getRepresentative rs = head (fromRanges $ R.invert rs) - 1
-- Given an input character, get the character that represents it.
mapToRepresentative :: Encoding -> Int -> Int
mapToRepresentative enc i = getRepresentative $ fromJust $ find (`R.inRanges` i) enc

-- Printing helper.
intToChar :: Int -> String
intToChar 136756 = "Top"
intToChar 0      = "NULL"
intToChar (-1)   = "ε"
intToChar (-2)   = "^"
intToChar (-3)   = "$"
intToChar (-4)   = "Bottom"
intToChar c      = [chr c]