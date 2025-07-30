-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.FaOperations
-- Copyright   :  (c) João Saraiva 2001,2002,2003,2004,2005,2017,2025
-- License     :  LGPL
--
-- Maintainer  :  saraiva@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions manipulating Finite Automata (DFA and NDFA)
--
-- Code Included in the Lecture Notes on
--      Language Processing (with a functional flavour).
--
-----------------------------------------------------------------------------

module Language.HaLex.FaOperations (
                      ndfa2dfa
                    , dfa2ndfa
                    , ndfa2ct
                    , CT
                    , lookupCT
                    , stsDfa
                    , concatNdfa
                    , unionNdfa
                    , starNdfa
                    , plusNdfa
                    , expNdfa
                    , unionDfa
                    , concatDfa
                    , starDfa
                    , plusDfa
                    ) where

import Data.List
import Language.HaLex.Util
import Language.HaLex.Dfa
import Language.HaLex.Ndfa



--
-- | Making a 'Dfa' from a 'Ndfa'
--

-- | The states of a 'Dfa' resulting from a 'Ndfa' are sets of
--   states of the 'Ndfa'

type StDfa st = [st]


-- | A transition table will be used to transform a 'Ndfa' into a 'Dfa'
--

--type CT st =  TableDfa (StDfa st)       -- [( StDfa st, [StDfa st])]

type CT st = [( StDfa st, [StDfa st])]
stsDfa :: CT st -> [StDfa st]
stsDfa   = map fst

stsRHS :: CT st -> [[StDfa st]]
stsRHS   = map snd

allstsCT :: Eq st => CT st -> [StDfa st]
allstsCT = nub . concat . stsRHS



-- | From a 'Ndfa' to a 'Dfa'
-- 

ndfa2dfa :: (Ord st,Eq sy)
         => Ndfa st sy               -- ^ Nondterminitic Finite Automaton
         -> Dfa [st] sy              -- ^ Deterministic Finite Automaton
ndfa2dfa ndfa@(Ndfa v q s z delta)  = (Dfa v' q' s' z' delta')
  where  tt = ndfa2ct ndfa
         v' = v
         q' = stsDfa tt
         s' = fst (head tt)
         z' = finalStatesDfa q' z
         delta' st sy = lookupCT st sy tt v


finalStatesDfa :: Eq st => [StDfa st] -> [st] -> [StDfa st]
finalStatesDfa []     _ = []
finalStatesDfa (q:qs) z | (not . null) (q `intersect` z)   = q : finalStatesDfa qs z
                        | otherwise                        = finalStatesDfa qs z


-- | Lookup the Transition Table 'CT' of a the resulting 'Dfa'
--

lookupCT :: (Eq st, Eq sy)
         => StDfa st         -- ^ Origin state of the 'Dfa' 
         -> sy               -- ^ Symbol
         -> CT st            -- ^ Transition Table of the 'Dfa'
         -> [sy]             -- ^ Vocabulary 
         -> StDfa st         -- ^ Destination state of the 'Dfa'
lookupCT st sy []     v  = []
lookupCT st sy (q:qs) v  | (fst q == st) = (snd q) !! col
                         | otherwise     = lookupCT st sy qs v
   where (Just col) = elemIndex sy v

-- | Compute the 'Dfa' transition table giving a 'Ndfa'
-- 

ndfa2ct :: Ord st
        => Ndfa st sy             -- ^ Nondterminitic Finite Automaton
        -> CT st                  -- ^ Transition Table
ndfa2ct (Ndfa v q s z delta) = limit (ndfa2dfaStep delta v) ttFstRow
  where  ttFstRow = newRows delta [epsilon_closure delta s] v


ndfa2dfaStep :: Ord st => (st -> (Maybe sy) -> [st]) -> [sy] -> CT st -> CT st
ndfa2dfaStep delta alfabet ct = nub (ct `union` newRows delta newSts alfabet)
  where newSts =  (allstsCT ct) <-> (stsDfa ct)


newRows :: Ord st => (st -> (Maybe sy) -> [st]) -> [StDfa st] -> [sy] -> CT st
newRows delta sts alfabet = map (\ st -> (st, newRow delta st alfabet)) sts


newRow :: Ord st => (st -> (Maybe sy) -> [st]) -> (StDfa st) -> [sy] -> [StDfa st]
newRow delta sts alfabet = map (\ v -> sort (ndfawalk delta sts [v])) alfabet



ndfa2ct' :: Ord st => Ndfa st sy -> CT st
ndfa2ct' (Ndfa v q s z delta) =
         fst $ ndfa2ctstep' delta v [] [fstState] []
  where  fstState = epsilon_closure delta s


ndfa2ctstep' :: Ord st
             => (st -> Maybe sy -> [st]) -> [sy] -> CT st
             -> [StDfa st] -> [StDfa  st] -> (CT st , [StDfa st])
ndfa2ctstep' delta v ct []       done = (ct  , done )
ndfa2ctstep' delta v ct (st:sts) done = (ct'' , done'')
    where   done'  = st : done
            newRow' = (st , newRow delta st v)
            ct'    = newRow' : ct
            newSts =  (snd newRow') <-> done'
            worker = sts ++ newSts
            (ct'' , done'' ) = ndfa2ctstep' delta v ct' worker done'


-- | Making a 'Ndfa' from a 'Dfa'
--

dfa2ndfa :: Dfa st sy -> Ndfa st sy
dfa2ndfa (Dfa v q s z delta) = (Ndfa v q [s] z delta')
  where delta' q (Just a) = [delta q a]
        delta' q Nothing  = []



-----------------------------------------------------------------------------
-- * Combining Finite Automata


-- | Concatenation of Ndfa's
--

concatNdfa :: (Eq a, Eq b) => Ndfa b a -> Ndfa b a -> Ndfa b a
concatNdfa (Ndfa vp qp sp zp dp) (Ndfa vq qq sq zq dq) = Ndfa v' q' s' z' d'
  where v' = vp `union` vq
        q' = qp `union` qq
        s' = sp
        z' = zq
        d' q | q `elem` zp = dp' q
             | q `elem` qp = dp  q
             | otherwise   = dq  q
         where dp' q Nothing = (dp q Nothing) `union` sq
               dp' q sy      = dp q sy

-- | Union of 'Ndfa'
--

unionNdfa :: (Eq a, Eq b) => Ndfa b a -> Ndfa b a -> Ndfa b a
unionNdfa (Ndfa vp qp sp zp dp) (Ndfa vq qq sq zq dq) = Ndfa v' q' s' z' d'
  where v' = vp `union` vq
        q' = qp `union` qq
        s' = sp `union` sq
        z' = zp `union` zq
        d' q | q `elem` qp = dp q
             | q `elem` qq = dq q

-- | Star of 'Ndfa'
--

starNdfa :: Eq st => Ndfa st sy -> Ndfa st sy
starNdfa (Ndfa v qs s z d) = Ndfa v qs s z d'
  where d' q | q `elem` s = ds' q
             | q `elem` z = dz' q
             | otherwise  = d q
          where ds' q Nothing  = z `union` (d q Nothing)
                ds' q sy       = d q sy

                dz' q Nothing  = s `union` (d q Nothing)
                dz' q sy       = d q sy

-- | Plus of 'Ndfa'
--

plusNdfa :: Eq st => Ndfa st sy -> Ndfa st sy
plusNdfa (Ndfa v qs s z d) = Ndfa v qs s z d'
  where d' q | q `elem` z = dz' q
             | otherwise  = d q
          where dz' q Nothing  = s `union` (d q Nothing)
                dz' q sy       = d q sy

-- | Exponenciation of 'Ndfa' 
--

expNdfa :: (Eq st,Eq sy) => Ndfa st sy -> Int -> Ndfa Int sy
expNdfa ndfa n = expNdfa' (renameNdfa ndfa 1) n

expNdfa' :: Eq sy => Ndfa Int sy -> Int -> Ndfa Int sy
expNdfa'  ndfa 1 = ndfa
expNdfa'  ndfa i = concatNdfa ndfa (expNdfa' ndfa (i-1))


-- | Concatenation of 'Dfa'
--

concatDfa :: (Eq a, Eq b) => Dfa b a -> Dfa b a -> Ndfa b a
concatDfa (Dfa vp qp sp zp dp) (Dfa vq qq sq zq dq) = Ndfa v' q' s' z' d'
  where v' = vp `union` vq
        s' = [sp]
        z' = zq
        q' = qp `union` qq
        d' q | q `elem` zp = dz' q
             | q `elem` qp = dp' q
             | q `elem` qq = dq' q
         where dz' q  Nothing = [sq]
               dz' q (Just y) | y `elem` vp = [dp q y]
                              | otherwise   = []

               dp' q Nothing  = []
               dp' q (Just y) | y `elem` vp = [dp q y]
                              | otherwise   = []

               dq' q Nothing  = []
               dq' q (Just y) | y `elem` vq = [dq q y]
                              | otherwise   = []

-- | Union of 'Dfa'
--

unionDfa :: (Eq a, Eq b) => Dfa b a -> Dfa b a -> Ndfa b a
unionDfa (Dfa vp qp sp zp dp) (Dfa vq qq sq zq dq) = Ndfa v' q' s' z' d'
  where v' = vp `union` vq
        q' = qp `union` qq
        s' = [sp,sq]
        z' = zp ++ zq
        d' _ Nothing                 = []
        d' q (Just sy) | q `elem` qp && sy `elem` vp = [dp q sy]
                       | q `elem` qq && sy `elem` vq = [dq q sy]
                       | otherwise   = []

-- | Star 'Dfa'
--

starDfa :: Eq st => Dfa st sy -> Ndfa st sy
starDfa (Dfa v q s z d) = Ndfa v q [s] z d'
  where d' q | q == s     = ds' q
             | q `elem` z = dz' q
             | otherwise  = dd' q
          where ds' q Nothing  = z
                ds' q (Just y) = [d q y]

                dz' q Nothing  = [s]
                dz' q (Just y) = [d q y]

                dd' q (Just y) = [d q y]
                dd' _ _        = []

-- | Plus pf 'Dfa'
--

plusDfa :: Eq st => Dfa st sy -> Ndfa st sy
plusDfa (Dfa v q s z d) = Ndfa v q [s] z d'
  where d' q | q `elem` z = dz' q
             | otherwise  = dd' q
          where dz' q Nothing  = [s]
                dz' q (Just y) = [d q y]

                dd' q (Just y) = [d q y]
                dd' _ _ = []
