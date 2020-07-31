{-# LANGUAGE DeriveGeneric #-}

module DFA where

import GHC.Generics (Generic)
import Constants
import Grammar
import Data.Hashable
import qualified Data.HashMap.Strict as M 
import qualified Data.HashSet as S
import Derive
import Simplifier

-- The representation of a state : a RE which is assumed to be in canonical form.
type States = S.HashSet RE

newtype TransitionKey = TransitionKey { k :: (Int, RE) } deriving (Eq, Generic)

newtype Transition = Transition { internalMap :: M.HashMap TransitionKey RE } deriving (Eq, Generic)
-- The representation of a transition. The keys are all of the form (TransitionConcat i s), where i is some input RE and s is some state.



-- The representation of a DFA: (all states, initial state, accepting states, transition mapping)
type DFA = (States, RE, States, Transition)


type DFAWithCClassesEncoding = (DFA, Encoding)

instance Hashable TransitionKey

instance Hashable CRE

instance Show TransitionKey where
    show (TransitionKey (n, re)) = (intToChar n) ++ " + " ++ show re

instance Show Transition where
    show (Transition internalMap) = M.foldlWithKey' (\acc k v -> acc ++ "\n " ++ (show k) ++ " => " ++ (show v)) "" internalMap 

-- The representation of a transition. The keys are all of the form (TransitionConcat i s), where i is some input RE and s is some state.



-- The representation of a DFA: (all states, initial state, accepting states, transition mapping)
--type DFA = (States, RE, States, Transition)


--type DFAWithCClassesEncoding = (DFA, Encoding)


-- Given a regular expression, return an automaton that recognizes it.
mkDFA :: CRE -> DFAWithCClassesEncoding
mkDFA q = ( (s, q, a, (Transition t')), cclasses )
                where
                    t' = M.filter (/= cVoid) (internalMap t)
                    a = S.filter (isNullable) s
                    (s, t) = explore (S.empty, (Transition M.empty)) q $ map getRepresentative cclasses
                    cclasses = deriveEncoding q

-- A simplified DFA with only the set of states and the transition mapping.
type RawDFA = (States, Transition)

explore :: RawDFA -> CRE -> [Int] -> RawDFA
explore initialDFA q reps = foldl (\newDFA input -> goto newDFA q' input reps) initialDFA reps
						where
							q' = mkCanon q


-- The Int is supposed to be the first element of a rage, its representative element
goto :: RawDFA -> CRE -> Int -> [Int] -> RawDFA
goto (s, (Transition t)) q input reps   | S.member qc s = (s, (Transition t'))
                                        | otherwise = explore (s', (Transition t')) qc reps
                                        where
                                            t' = M.insert (TransitionKey (input, q)) qc t
                                            s' = S.insert qc s
                                            qc = mkCanon $ derive q input


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