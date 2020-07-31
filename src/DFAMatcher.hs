module DFAMatcher where

import Grammar
import DFACore
import Data.Maybe
import Data.Range.Range
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Range.Range as R
import Debug.Trace

matchLine dfa@(_, _, a, t) (i:is) state index la = matchLine' dfa (i:is) state index la



-- Match as much as possible and return the rest of the input.
matchLine' :: DFA -> [Int] -> CRE -> Int -> Int-> Int
matchLine' (_, _, a, t) [i] state index la 	| S.member (findNextState t state i ) a = 
												--trace ("matchLine BASECASE acc : " ++ show index) 
												index
											| otherwise = 
												--trace ("matchLine BASECASE fail : " ++ show la) 
												la
matchLine' dfa@(_, _, a, t) (i:is) state index la	| S.member state' a = 
														--trace ("matchLine " ++ (intToChar i) ++ " + " ++ show state ++ " => " ++ show state') 
														matchLine' dfa is state' index' index
													| state' == CVoid = 
														--trace ("matchLine " ++ (intToChar i) ++ " + " ++ show state ++ " => " ++ show state') 
														la
													| otherwise = 
														--trace ("matchLine " ++ (intToChar i) ++ " + " ++ show state ++ " => " ++ show state')
														matchLine' dfa is state' index' la
												where
													state' = findNextState t state i
													index' 	| i >= 0 = index + 1
															| otherwise = index

-- Find the next state given a current state and an input char.
findNextState :: Transition -> CRE -> Int -> CRE
findNextState (Transition t) state input = case M.lookup (TransitionKey (input, state)) t of
                                    (Just re) -> re
                                    Nothing -> cVoid