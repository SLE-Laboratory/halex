
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.Dfa
-- Copyright   :  (c) João Saraiva 2001,2002,2003,2004,2005, 2016
-- License     :  LGPL
--
-- Maintainer  :  jas@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- Parsing (through parsing combinators) concrete regular Expressions
--                (in the Unix like notation)
--
-- Code Included in the Lecture Notes on
--      Language Processing (with a functional flavour).
--
-----------------------------------------------------------------------------


module Language.HaLex.RegExpParser ( parseRegExp
                                   ) where

import Prelude hiding ((<$>), (<*>))
import Data.Char
import Language.HaLex.Parser
import Language.HaLex.RegExp


-- | Parser for regular expressions

parseRegExp :: [Char]                -- ^ Input symbols
            -> Maybe (RegExp Char)   -- ^ Regular expression
parseRegExp re = res
  where parsed_re = expr re
        res | parsed_re == [] = Nothing
            | otherwise       = Just (fst (head parsed_re))

expr :: Parser Char (RegExp Char)
expr =  f  <$> termThen <*> symbol '|' <*> expr
    <|> id <$> termThen
    <|> succeed Epsilon
  where f l _ r = Or l r

termThen :: Parser Char (RegExp Char)
termThen =  f  <$> term <*> termThen
        <|> id <$> term
  where f l r   = Then l r

term :: Parser Char (RegExp Char)
term =  f  <$> factor <*> symbol '?'
    <|> g  <$> factor <*> symbol '*'
    <|> h  <$> factor <*> symbol '+'
    <|> id <$> factor
  where
     f e _ = Or   e Epsilon
     g e _ = Star e
     h e _ = Then e (Star e)

factor :: Parser Char (RegExp  Char)
factor =  f <$> letterOrDigit
      <|> g <$> symbol '\'' <*> satisfy (\ x -> True) <*> symbol '\''
      <|> h <$> symbol '(' <*> expr <*> symbol ')'
      <|> k <$> symbol '[' <*> (oneOrMore range) <*> symbol ']'
      <|> l <$> symbol '[' <*> symbol '^' <*> range <*> symbol ']'
  where
     f a         = Literal a
     g _ e _     = Literal e
     h _ e _     = e
     k _ l _     = RESet (concat l)
     l _ _ l _   = RESet [ x | x <- ascii
                         , not (x `elem` l)
                         ] 

range :: Parser Char [Char]
range  =  f   <$> letterOrDigit <*> symbol '-' <*> letterOrDigit
      <|> id  <$> oneOrMore (satisfy (\ x -> x `elem` ascii
                                             && x /= '-' && x /= '^'))
  where f a _ c = [a..c]

      

letterOrDigit :: Parser Char Char
letterOrDigit = satisfy (\x -> isDigit x || isAlpha x)

setRegExp :: Char                -- ^ first elem of the set
          -> Char                -- ^ last elem of the set
          -> RegExp Char         -- ^ Regular Expression for the set
setRegExp a b = foldr1 Or (map Literal [a..b])



-- Ascii characteres (C Language)
ascii :: [Char]
ascii =  ['a'..'z']                             -- lower letter
      ++ ['A'..'Z']                             -- capital letters
      ++ [' ','\n','\t']                        -- Tab Or New line Or Space
      ++ "~|#$%^&*)(_+|\\`-={}[]:\";<>?,./"     -- Special Characters



-- Not used yet
spaces :: Parser Char ()
spaces = (\ _ _ -> ()) <$> symbol ' ' <*> spaces
      <|> succeed ()
