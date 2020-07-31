module DFABuilder where

import Grammar
import DFACore
import Derive
import Constants
import Simplifier
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Char
import Data.Range.Range as R
import Debug.Trace
import Data.List
import Data.Maybe as MB



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