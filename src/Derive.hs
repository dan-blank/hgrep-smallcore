module Derive where

import Grammar
import Constants
import Simplifier
import Data.Range.Range as R
import Data.Char
import Data.List
import Debug.Trace

emptyset = [R.SingletonRange (-1::Int)]

-- Given a state and an input, return the derived state. We assume that there is no empty input.
derive :: RE -> Int -> RE
derive re int = 
    --(\ res -> trace ("derive>> "  ++ (show . intToChar) int ++ " + " ++ show re ++ " ::= " ++ (show $ mkCanon res)) res) $ 
    derive' re int

derive' :: RE -> Int -> RE
derive' (CConcat l r) i     | (not . isNullable) l = leftderivate
                            | otherwise = CUnion leftderivate (CConcat (toEmptyOrVoid l) (derive' r i))
                        where
                            leftderivate = CConcat (derive' l i) r
derive' (CStar re) i = CConcat (derive' re i) (CStar re)
derive' (CUnion l r) i = CUnion (derive' l i) (derive' r i)
derive' CVoid i = cVoid
derive' (CSet (CRange lranges)) i | R.inRanges lranges i = cEmpty
                                  | i == -2 = (CSet (CRange lranges))
                                  | i == -3 = (CSet (CRange lranges))
                                  | otherwise = cVoid

-- Todo: Explain
deriveEncoding :: CRE -> Encoding
deriveEncoding cre = [[R.SingletonRange (-2::Int)], [R.SingletonRange (-3::Int)]] ++ (Data.List.nub $ filter (/= []) $ deriveCHelper cre)
                    

deriveCHelper :: CRE -> [[Range Int]]
deriveCHelper (CSet (CRange ranges))    = ranges : [R.difference [alphabet] ranges]    
deriveCHelper (CStar re)                = deriveCHelper re
deriveCHelper (CUnion l r)              = filter (/= []) [R.intersection left right | left <-l', right <-r']
                        where
                            l' = deriveCHelper l
                            r' = deriveCHelper r
deriveCHelper (CConcat l r)  = [R.intersection left right | left <-l', right <-r']
							where
                                l' = deriveCHelper l
                                r' = deriveCHelper r


toEmptyOrVoid :: RE -> RE
toEmptyOrVoid re    | isNullable re = cEmpty
                    | otherwise = cVoid

isNullable :: RE -> Bool
isNullable (CSet (CRange rng)) = R.inRanges rng (-1)
isNullable (CStar re) = True
isNullable (CConcat l r) = isNullable l && isNullable r
isNullable (CUnion l r) = isNullable l || isNullable r