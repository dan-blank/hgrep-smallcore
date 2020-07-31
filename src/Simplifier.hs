module Simplifier where

import Grammar
import Constants
import Data.Range.Range as R

-- Transform a RE into an RE that is in canonical form. Two REs which are equivalent should result in the same canonical forms.
mkCanon :: RE -> RE
mkCanon re  | re == re' = re
            | otherwise = mkCanon re'
            where
                re' = mkCanon' re

-- Simplify one step
mkCanon' :: RE -> RE
mkCanon' (CUnion CVoid r) = mkCanon' r
mkCanon' (CUnion r CVoid) = mkCanon' r
mkCanon' (CUnion CAny r) = CAny
mkCanon' (CUnion r CAny) = CAny
mkCanon' (CUnion (CSet (CRange l)) (CSet (CRange r))) = CSet $ CRange $ R.mergeRanges $ l ++ r
mkCanon' (CUnion l r)   | l' == r' = l'
                        | l' < r' = CUnion l' r'
                        | otherwise = CUnion r' l'
                    where
                        l' = mkCanon' l
                        r' = mkCanon' r
mkCanon' (CUnion (CUnion l1 l2) r)  | l2' == r' && l1' == r'= r'
									| l2' == r' = CUnion l1' r'
									| l1' == r' = CUnion l2' r'
									where
										r' = mkCanon' r
										l1' = mkCanon' l1
										l2' = mkCanon' l2 
mkCanon' (CConcat _ CVoid) = cVoid
mkCanon' (CConcat CVoid _) = cVoid
mkCanon' (CConcat l (CSet (CRange []))) = mkCanon' l
mkCanon' (CConcat (CSet (CRange [])) r) = mkCanon' r
mkCanon' (CConcat (CConcat l1 (CStar l2)) (CStar r))	| l2' == r' = CConcat (mkCanon l1) (CStar l2')
													where
														r' = mkCanon' r
														l2' = mkCanon' l2
mkCanon' (CConcat (CStar l) (CConcat (CStar r1) r2))	| l' == r1' = CConcat (CStar l) (mkCanon r2)
												where
													r1' = mkCanon' r1
													l' = mkCanon' l
mkCanon' (CConcat r CEmpty) = mkCanon' r
mkCanon' (CConcat CEmpty r) = mkCanon' r
mkCanon' (CConcat (CStar l) (CStar r)) | l' == r' = (CStar l') -- Prevents looping for (.*.*)
                                where
                                    l' = mkCanon' l
                                    r' = mkCanon' r
mkCanon' (CConcat l r) = CConcat (mkCanon' l) (mkCanon' r)
mkCanon' (CStar (CSet (CRange []))) = cEmpty
mkCanon' (CStar CEmpty) = cEmpty
mkCanon' (CStar CVoid) = cEmpty
mkCanon' (CStar (CStar re)) = CStar $ mkCanon' re
mkCanon' (CStar re) = CStar $ mkCanon' re
mkCanon' (CSet (CRange [R.SpanRange n m])) | m == n = (CSet (CRange [R.SingletonRange n]))
mkCanon' re = re