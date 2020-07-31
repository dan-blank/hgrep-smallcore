module ParseStringIntoTokens where

import Grammar

-- Parses a String into an array of tokes which are already excaped.
-- "^ab"    => [(TAnchorBeg),(TChar 'a'),(TChar 'b')]
-- "\^ab"   => [(TChar '^'),(TChar 'a'),(TChar 'b')]
parseTokens :: String -> Bool -> [Token]
parseTokens [] _ = []
parseTokens (c:cs) True     = TChar c : parseTokens cs False
parseTokens ('\\':cs) False = parseTokens cs True
parseTokens (c:cs) False    = mkToken c : parseTokens cs False

mkToken :: Char -> Token            
mkToken '$'  = TAnchorEnd
mkToken '^'  = TAnchorBeg
mkToken '(' = TPL
mkToken ')' = TPR
mkToken '[' = TBPL
mkToken ']' = TBPR
mkToken '{' = TCPL
mkToken '}' = TCPR
mkToken '?' = TQM
mkToken '+' = TPlus
mkToken '.' = TDot
mkToken ',' = TComma
mkToken '|' = TPipe
mkToken '*' = TStar
mkToken c = TChar c