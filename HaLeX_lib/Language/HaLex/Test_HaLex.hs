-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.Sentences
-- Copyright   :  (c) João Saraiva 2017
-- License     :  LGPL
--
-- Maintainer  :  jas@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- Generation of sentences of the (regular) languages defined via
--   Regular Expressions and Finite Autumata
--
-----------------------------------------------------------------------------

module Language.HaLex.Test_HaLex ( test_size_fa
                                 , test_gen_sentences
                                 ) where

import Language.HaLex.Dfa
import Language.HaLex.Ndfa
import Language.HaLex.RegExp
import Language.HaLex.RegExp2Fa
import Language.HaLex.RegExpParser
import Language.HaLex.FaOperations
import Language.HaLex.Minimize
import Language.HaLex.Sentences
import Language.HaLex.FaClasses

import Data.Maybe
import Data.List

import Test.HUnit



-----------------------------------------------------------------------------
-- * Generic functions to test regular expressions and finite automata


-- | Test the size of finite automata
--   The size (ie number of states) of a minimized dfa is always
--   less of equal than the size of an equivalent ndfa or dfa.

test_size_fa :: (Ord st, Ord sy, Show st, Show sy)
             => Ndfa st sy -> Test
test_size_fa ndfa =  TestList [ sizeFa dfa_min <= sizeFa dfa  ~?= True
                              , sizeFa dfa_min <= sizeFa ndfa ~?= True
                              ]
         where dfa     =  ndfa2dfa   ndfa
               dfa_min =  minimizeDfa dfa


-- | Test the acceptance of generated sentences
--   The accpetance functions for 'RegExp', 'Ndfa' and 'Dfa' should
--   accept all sentences of the language of an equivalent reg. exp.

test_gen_sentences :: (Ord sy, Show sy) => RegExp sy -> Test

test_gen_sentences re =
    TestList [ and (map (matches      re)  sentences_re) ~?= True
             , and (map (accept     ndfa)  sentences_re) ~?= True
             , and (map (accept      dfa)  sentences_re) ~?= True
             , and (map (accept  dfa_min)  sentences_re) ~?= True
             ]
       where sentences_re = sentencesRegExp re
             ndfa         = regExp2Ndfa re
             dfa          = ndfa2dfa ndfa
             dfa_min      =  minimizeDfa dfa



-----------------------------------------------------------------------------
-- * Examples



re = fromJust $ parseRegExp "('+'|'-')?[0-9]*('.'?)[0-9]+"


re'' = fromJust $ parseRegExp "a[^a]*a"


ndfa     = regExp2Ndfa re
dfa      = ndfa2dfa ndfa
dfa_int  = beautifyDfa dfa

dfa_min  = minimizeDfa dfa_int
dfa_min' = beautifyDfa dfa_min


test_acceptNdfa = TestList [ ndfaaccept ndfa "109"    ~?= True
                           , ndfaaccept ndfa "+13"    ~?= True
                           , ndfaaccept ndfa "-13.4"  ~?= True
                           , ndfaaccept ndfa "-.15"   ~?= True
                           , ndfaaccept ndfa "+0.123" ~?= True
                           , ndfaaccept ndfa "-.2.3"  ~?= False
                           , ndfaaccept ndfa ""       ~?= False
                           ]

test_acceptDfa  = TestList [ dfaaccept dfa "109"    ~?= True
                           , dfaaccept dfa "+13"    ~?= True
                           , dfaaccept dfa "-13.4"  ~?= True
                           , dfaaccept dfa "-.15"   ~?= True
                           , dfaaccept dfa "+0.123" ~?= True
                           , dfaaccept dfa "-.2.3"  ~?= False
                           , dfaaccept dfa ""       ~?= False
                           ]

test_acceptDfamin  = TestList [ dfaaccept dfa_min "109"    ~?= True
                              , dfaaccept dfa_min "+13"    ~?= True
                              , dfaaccept dfa_min "-13.4"  ~?= True
                              , dfaaccept dfa_min "-.15"   ~?= True
                              , dfaaccept dfa_min "+0.123" ~?= True
                              , dfaaccept dfa_min "-.2.3"  ~?= False
                              , dfaaccept dfa_min ""       ~?= False
                              ]


dfaToHaskell = toHaskell dfa_int "Dfa_RE"


re'  = fromJust $ parseRegExp "[a-z][a-z]*"



main = do runTestTT test_acceptNdfa
          runTestTT test_acceptDfa
          runTestTT test_acceptDfamin
          runTestTT $ test_size_fa  (regExp2Ndfa re)
          runTestTT $ test_size_fa  (regExp2Ndfa re')
          runTestTT $ test_gen_sentences re
          runTestTT $ test_gen_sentences re'



