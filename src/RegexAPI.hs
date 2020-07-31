{-# LANGUAGE LambdaCase #-}   

module RegexAPI where

import Grammar
import Constants
import DFACore
import ParseStringIntoTokens
import ParseTokensIntoRawAST
import ParseRawASTIntoAST
import Simplifier
import DFABuilder
import DFAMatcher
import IREtoCRE
import Derive
import qualified Data.HashSet as S
import Debug.Trace
import Data.Char
import Data.List
import Data.Maybe
import Data.Range.Range as R
import String
import Terminal

----------------------------------------------
--- API
----------------------------------------------

parseStringToCRE :: String -> Maybe RE
parseStringToCRE s = case parseTokensIntoRawAst $ parseTokens s False of
                    (Just rawRe) -> Just $ mkCanon $ t_IREtoCRE $ tRaw $ fromJust $ parseTokensIntoRawAst $ parseTokens s False
                    Nothing -> Nothing

-- Given a redex string, return an automaton that recognizes that redex.
parseRegex :: String -> Maybe DFAWithCClassesEncoding
parseRegex s = case parseStringToCRE s of
                    (Just cre) -> Just $ mkDFA $ cre
                    Nothing -> Nothing

-- Given an automaton and a string, return a List of (Begin, End)-Indixes that denote where the string matched the automaton.
parseLine :: DFAWithCClassesEncoding -> String -> [(Int, Int)]
parseLine dwe@(_, enc) s = reduceListFromLeft $ parseLines dwe ((-2):(map (\c -> mapToRepresentative enc (ord c)) s) ++ [-3]) 0

parseLines :: DFAWithCClassesEncoding -> [Int] -> Int -> [(Int, Int)]
parseLines (dfa@(_, initial, _, _), enc) inputs index = 
	filter (/= ((-1),(-1))) $
	map (\(n, s) -> parseSubLine s n) 
		$ zip [0,1..] 
		$ listsToSublists inputs
	where
		parseSubLine is start = 
			case matchLine dfa is initial 0 (-1) of
				-1 -> (-1, -1)
				offset -> (start, start + offset)


----------------------------------------------
--- Helpers
----------------------------------------------

reduceListFromLeft = consumeFromLeft . sortBy smallFirstBigSecond

smallFirstBigSecond (a1,b1) (a2,b2) = compare a1 a2 `mappend` compare b2 b1

consumeFromLeft :: [(Int, Int)] -> [(Int, Int)]
consumeFromLeft [] = []
consumeFromLeft [m] = [m]
consumeFromLeft ((lb,le):(rb,re):ms)	| le >= rb || le >= re = consumeFromLeft ((lb,le):ms)
										| otherwise = (lb,le) : consumeFromLeft ((rb,re):ms)
										
listsToSublists :: [a] -> [[a]]
listsToSublists [] = []
listsToSublists (c:s) = [(c:s)] ++ listsToSublists s

----------------------------------------------
-- Helper
----------------------------------------------

lineMatches :: DFAWithCClassesEncoding -> String -> Bool
lineMatches dfa line = case parseLine dfa line of
                [] -> False
                [(-1,-1)] -> False
                _ -> True

highlightLines :: DFAWithCClassesEncoding -> [String] -> Bool -> [String]
highlightLines dfa ls b = map (\ s -> highlightLine dfa s b) $ (filter (lineMatches dfa) ls)

highlightLine :: DFAWithCClassesEncoding -> String -> Bool -> String
highlightLine dfa line b = partiallyMapString line (sort $ map incTuple (parseLine dfa line)) defaultHighlight b
    where 
        incTuple (l, r) = (l, r + 1) -- Right index not included in highlighting
            
          