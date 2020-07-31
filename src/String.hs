module String where

import Data.Range.Range as R

listToRanges :: [(Int,Int)] -> [Range Int]
listToRanges [] = []
listToRanges ((lb, le):xs) = R.SpanRange lb le : listToRanges xs

rangesToList :: [Range Int] -> [(Int,Int)]
rangesToList [] = []
rangesToList ((R.SpanRange n m):xs) = (n, m) : rangesToList xs
rangesToList ((R.SingletonRange s):xs) = (s, s) : rangesToList xs

invertMatches :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
invertMatches lb ub ms = rangesToList $ R.intersection [R.SpanRange lb ub] $ R.invert $ listToRanges ms


{-|
  Splits the string into two strings at the specified index
-}
binarySplit :: String -> Int -> (String, String)
binarySplit string index = (take index string, drop index string)


{-|
  Splits the string into three strings at the specified indices
-}
ternarySplit :: String -> Int -> Int -> (String, String, String)
ternarySplit string start end = (left, middle, right)
    where
        (left, right') = binarySplit string start
        (middle, right) = binarySplit right' (end - start)


{-|
  Maps only substrings captured by indices in the specified tuples
-}

partiallyMapString :: String -> [(Int, Int)] -> (String -> String) -> Bool -> String
partiallyMapString string ls f False = partiallyMapString' string ls f
partiallyMapString string ls f True = partiallyMapString' string inverted f
  where inverted = invertMatches 0 (length string) ls

partiallyMapString' :: String -> [(Int, Int)] -> (String -> String) -> String
partiallyMapString' string [] f = string
partiallyMapString' string ((start, end) : ts) f = left ++ (f middle) ++ (partiallyMapString' right ts' f)
    where
        (left, middle, right) = ternarySplit string start end -- split the current string
        ts' = map
                ( \(l, r) -> (l - end, r - end) ) -- reduce all the following indices
                ts
