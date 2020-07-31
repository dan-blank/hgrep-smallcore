module IREtoCRE where

import Grammar
import Constants
import Data.Range.Range as R
import Data.Char


bigLetters = R.SpanRange 65 90
smallLetters = R.SpanRange 97 122
digits = R.SpanRange 48 57
underscore = R.SingletonRange 95
word = CSet $ CRange $ R.mergeRanges [bigLetters, smallLetters, digits, underscore]
--wordinv = CSet $ CRange $ R.invert $ R.mergeRanges [bigLetters, smallLetters, digits, underscore]
--anchorSmallB = CUnion (CConcat word cEmpty) (CConcat cEmpty word)
--anchorSmallBinv = CUnion (CConcat wordinv cEmptyInv) (CConcat cEmptyInv wordinv)

t_IREtoCRE :: IRE -> CRE
t_IREtoCRE = translateForced

translateForced (Union l r) = CUnion (translateForced l) (translateForced r)
translateForced (Concat l r) = CConcat (translateForced l) (translateForced r)
translateForced (Star re) = CStar $ translateForced re
--translateForced (Complement (Atom c)) = CSet $ CRange $ R.difference (R.invert $ [R.SingletonRange $ ord c]) [(R.SingletonRange (-1))] 
--translateForced (Complement re) = translateForced $ forceComplement re
translateForced (Atom c) = cSingleton $ ord c
translateForced (PositiveSet cs) = CSet $ CRange $ map (SingletonRange . ord) cs
--translateForced (NegativeSet cs) = CSet $ CRange $ R.intersection [(R.SpanRange 0 highestUnicode)] $ invert $ map (SingletonRange . ord) cs
--translateForced (Complement Any) = cVoid
translateForced Any = cAny
--translateForced (Complement Void) = cAny
--translateForced Void = cVoid
--translateForced (Complement Empty) = CSet $ CRange $ R.difference [alphabet] [R.SingletonRange (-1)]
translateForced Empty = cEmpty
--translateForced (Complement Bos) = cAnchorbegInv
translateForced Bos = cAnchorBeg
translateForced Eos = cAnchorEnd
{-|
translateForced (Complement Eos) = cAnchorendInv

translateForced (Complement Bow) = cAnchorbow
translateForced Bow = cAnchorbowInv
-}
{-|
forceComplement :: IRE -> IRE
forceComplement re = forceComplement' re False
-}


{-|
forceComplement' :: IRE -> Bool -> IRE
forceComplement' (Union l r) b = Union (forceComplement' l b) (forceComplement' r b)
forceComplement' (Concat l r) b = Concat (forceComplement' l b) (forceComplement' r b)
forceComplement' (Inter l r) b = Inter (forceComplement' l b) (forceComplement' r b)
forceComplement' (Star re) b = Star $ forceComplement' re b
forceComplement' (Complement re) True = forceComplement' re False
forceComplement' (Complement re) False = forceComplement' re True
forceComplement' re True = Complement re
forceComplement' re False = re
-}


