{-# LANGUAGE LambdaCase #-}

module ParseTokensIntoRawAST where

import Grammar
import ParserCon
import Data.Char


parseTokensIntoRawAst :: [Token] -> Maybe RawRE
parseTokensIntoRawAst = parse p_re

--------------------------------------
-- RE
--------------------------------------

p_re = (RASimple <$> p_simple) <|> p_union

p_union = RUnion <$> p_simple <* lit TPipe <*> p_re

--------------------------------------
-- Simple
--------------------------------------

p_simple = (RABasic <$> p_basic) <|> p_concatenation

p_concatenation = RConcat <$> p_basic <*> p_simple

--------------------------------------
-- Basic
--------------------------------------

p_basic = (RAnElementary <$> p_elementary) <|> p_star <|> p_plus <|> p_questionmark <|> p_ranged

p_star = RStar <$> p_elementary <* lit TStar

p_plus = RPlus <$> p_elementary <* lit TPlus

p_questionmark = RQestionmark <$> p_elementary <* lit TQM

p_ranged = RRanged <$> p_elementary <* lit TCPL <*> p_int <* lit TComma <*> p_int <* lit TCPR

--------------------------------------
-- Elementary
--------------------------------------

-- p_elementary = p_group <|> p_atom <|> p_any <|> p_eos <|> p_bos <|> p_negative_set <|> p_positive_set
p_elementary = p_group <|> p_atom <|> p_any <|> p_eos <|> p_bos <|> p_positive_set

p_group = RARE <$ lit TPL <*> p_re <* lit TPR

p_char = P $ \case
                ((TChar c):TComma:ts) -> [(c, ts)]
                ((TChar c):ts) -> [(c, ts)]
                _ -> []
p_any = P $ \case
                (TDot:ts) -> [(RAny, ts)]
                _ -> []
p_eos = P $ \case 
                (TAnchorEnd:ts) -> [(REos, ts)]
                _ -> []
p_bos = P $ \case
                (TAnchorBeg:ts) -> [(RBos, ts)]
                _ -> []
p_atom = RAtom <$> p_char

p_positive_set = RPositive_Set <$ lit TBPL <*> many1 p_char <* lit TBPR

--p_negative_set = RNegative_Set <$ lit TBPL <* lit TAnchorBeg <*> many1 p_char <* lit TBPR

p_sint :: Parser Token Char
p_sint = P $ \case
                ((TChar c):ts) -> case isDigit c of
                    True -> [(c, ts)]
                    False -> []
                _ -> []

p_int :: Parser Token Int
p_int =  read <$> many1 p_sint