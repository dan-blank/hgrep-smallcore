{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

module Grammar where

import GHC.Generics (Generic)
import Data.Range.Range as R
import Constants

type RE = CRE

----------------------------------------------
-- CRE: The final internal represention of a regex, where the smallest construct is a range of characters.
----------------------------------------------

data CRE =
    CUnion CRE CRE  | -- "a|b"
    CConcat CRE CRE | -- "ab" 
    CStar CRE       | -- "a*"
    CSet CRange       -- i.e. [Singleton 4] or [SpanRange 0 136756]
    deriving (Eq, Ord, Generic)

----------------------------------------------
-- RE: Simplified intermediate represention of a regex.
----------------------------------------------

data IRE =
    Union IRE IRE       | -- "a|b"
    Concat IRE IRE      | -- "ab" 
    Star IRE            | -- "a*" 
    Atom Char           |
    PositiveSet [Char]  |
    Any                 | -- "."
    Eos                 | -- "$"
    Bos                 | -- "^"
    Empty
    deriving (Eq)

----------------------------------------------
-- RawRE: Intermediate representation from tokens.
----------------------------------------------

data RawRE = 
    RUnion Simple_RE RawRE  |       -- a|b
    RASimple Simple_RE
    deriving (Eq, Show)
data Simple_RE = 
    RConcat Basic_RE Simple_RE  |   -- ab 
    RABasic Basic_RE
    deriving (Eq, Show)
data Basic_RE = 
    RRanged Elementary_RE Int Int   | -- a{12,43}   Must be simplified away
    RStar Elementary_RE             | -- a* 
    RPlus Elementary_RE             | -- a+b        Must be simplified away
    RQestionmark Elementary_RE      | -- a?         Must be simplified away
    RAnElementary Elementary_RE
    deriving (Eq, Show)
data Elementary_RE = 
    RPositive_Set [Char]    | -- [ab]               Must be simplified away
    RAtom Char              |
    RARE RawRE              | -- ( RE )
    RAny                    | -- .
    REos                    | -- $
    RBos                      -- ^
    deriving (Eq, Show)

----------------------------------------------
-- Token: Either a meta char such as '$' or a normal char. The first step in parsing a string into a regular expression.
----------------------------------------------

data Token = 
    TChar Char  |
    TAnchorEnd  |
    TAnchorBeg  |
    TPL         |
    TPR         |
    TBPL        |
    TBPR        |
    TCPL        |
    TCPR        |
    TQM         |
    TPlus       |
    TDot        |
    TComma      |
    TPipe       |
    TStar
    deriving (Eq, Show)

cEmpty = CSet $ CRange [(R.SingletonRange empty)]
cAnchorBeg = CSet $ CRange [(R.SingletonRange anchorBeg)]
cAnchorEnd = CSet $ CRange [(R.SingletonRange anchorEnd)]
cVoid = CVoid
cAny = CSet $ CRange [alphabet]
cSingleton i = (CSet (CRange [R.SingletonRange i]))
cSpan l r = (CSet (CRange [R.SpanRange l r]))

---------------------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------------------

instance Show CRE where
    show (CUnion l r) = (show l) ++ " | " ++ (show r)
    show (CConcat l r) = (show l) ++ " • " ++ (show r)
    show (CStar re) = show re ++ "*"
    show (CSet rs) = "[" ++ show rs ++ "]"

pattern SRLU n m = (CSet (CRange [LowerBoundRange m , UpperBoundRange n]))
pattern RLU n m rs = (CRange ((LowerBoundRange n):(UpperBoundRange m):rs))
pattern RSpan n m rs = (CRange ((R.SpanRange n m):rs))
pattern PVoid = CRange [LowerBoundRange 136756 , UpperBoundRange (-4)]
pattern CVoid = CSet PVoid
pattern PEmpty = CRange [SingletonRange (-1)]
pattern CEmpty = CSet PEmpty
pattern PAny = CRange [SpanRange (-3) 136755]
pattern CAny = CSet PAny

instance Show CRange where
    show (CRange ([]))                          = ""
    show (CRange ((R.SingletonRange n):rs))     = intToChar n ++ show (CRange rs) 
    show (RSpan (-3) 136755 rs)                 = "Σ" ++ show (CRange rs)
    show (RLU 136756 (-4) rs)                   = "Ø" ++ show (CRange rs)
    show (RSpan l r rs)                         = "[" ++ intToChar l ++ "," ++ intToChar r ++ "]" ++ show (CRange rs)
    show (CRange ((R.LowerBoundRange l):rs))    = "[" ++ intToChar l ++ ",∞]" ++ show (CRange rs)
    show (CRange ((R.UpperBoundRange r):rs))    = "[∞," ++ intToChar r ++ "]" ++ show (CRange rs)
    show (CRange ((R.InfiniteRange):rs))        = "∞" ++ show (CRange rs)