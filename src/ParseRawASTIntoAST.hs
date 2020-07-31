module ParseRawASTIntoAST where

import Grammar

-- Translates a raw AST into an AST that is simplier (uses less IREgex operations) and more compact.


tRaw :: RawRE -> IRE
tRaw (RUnion s r) = Union (tSimple s) (tRaw r)
tRaw (RASimple s) = tSimple s

tSimple :: Simple_RE -> IRE
tSimple (RConcat b s) = Concat (tBasic b) (tSimple s)
tSimple (RABasic b) = tBasic b

tBasic :: Basic_RE -> IRE
tBasic (RStar e) = Star $ tElementary e
tBasic (RAnElementary e) = tElementary e
tBasic (RPlus e) = Concat Empty $ tElementary e
tBasic (RQestionmark e) = Union Empty $ tElementary e
tBasic (RRanged e l r) = concatNTimesWithUnioning e' concatedMinimum $ r - l
					where
						concatedMinimum = (concatNTimes e' l)
						e' = tElementary e
tElementary :: Elementary_RE -> IRE
tElementary (RAtom c) = Atom c
tElementary (RARE r) = tRaw r
tElementary RAny = Any
tElementary REos = Eos
tElementary RBos = Bos
--tElementary (RNegative_Set cs) = NegativeSet cs
tElementary (RPositive_Set cs) = PositiveSet cs


-- helper functions

concatNTimes :: IRE -> Int -> IRE
concatNTimes r 1 = r
concatNTimes r n = Concat r (concatNTimes r $ n - 1)

concatNTimesWithUnioning :: IRE -> IRE -> Int -> IRE
concatNTimesWithUnioning append core 0 = core
concatNTimesWithUnioning append core n = Concat (Union append Empty) (concatNTimesWithUnioning append core $ n - 1)