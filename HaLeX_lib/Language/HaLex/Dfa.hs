{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.Dfa
-- Copyright   :  (c) João Saraiva 2001,2002,2003,2004,2005,2017
-- License     :  LGPL
--
-- Maintainer  :  jas@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- Deterministic Finite Automata in Haskell.
--
-- Code Included in the Lecture Notes on
--      Language Processing (with a functional flavour).
--
-----------------------------------------------------------------------------

module Language.HaLex.Dfa (
              -- * Data type
                Dfa (..)
              -- * Acceptance
              , dfaaccept
              , dfawalk
              -- * Transformation
              , TableDfa
              , ttAllSts
              , ttAllDestSts
              , ttDfa2Dfa
              , dfa2tdfa
              , complementDfa
              -- * Transitions
              , transitionsFromTo
              , destinationsFrom
              , transitionTableDfa
              , transitionTableDfa'
              , reachedStatesFrom
              -- * Printing
              , beautifyDfa
              , renameDfa
              , showDfaDelta
              , beautifyDfaWithSyncSt
              , dfaIO
              -- * Properties of 'Dfa'
              , sizeDfa
              , nodesAndEdgesDfa
              , nodesAndEdgesNoSyncDfa
              , dfadeadstates
              , dfasyncstates
              , cyclomaticDfa
              -- * Properties of States
              , isStDead
              , isStSync
              , numberOutgoingArrows
              , numberIncomingArrows
           ) where

import Data.List

import Language.HaLex.Util

-----------------------------------------------------------------------------
-- * Data type with the acceptance function

-- | The type of Deterministic Finite Automata parameterized with
--   the type @st@ of states and @sy@ of symbols.

data Dfa st sy = Dfa [sy]              -- Vocabulary
                     [st]              -- Finite set of states
                     st                -- The start state
                     [st]              -- The set of final states
                     (st -> sy -> st)  -- Transition Function


-----------------------------------------------------------------------------
-- * Acceptance


-- | Execute the transition function of a 'Dfa' on an initial state
--   and list of input symbol. Return the final state when all input
--   symbols have been consumed.

dfawalk :: (st -> sy -> st) -- ^ Transition function
        -> st               -- ^ Initial state
        -> [sy]             -- ^ Input symbols
        -> st               -- ^ Final state
dfawalk delta s [] = s
dfawalk delta s (x:xs) = dfawalk delta (delta s x) xs


-- | Test whether the given automaton accepts the given list of
--   input symbols.

dfaaccept' :: Eq st
           => Dfa st sy  -- ^ Automaton
           -> [sy]       -- ^ Input symbols
           -> Bool
dfaaccept' (Dfa v q s z delta) simb  = (dfawalk delta s simb) `elem` z


-- | Test whether the given automaton accepts the given list of
--   input symbols (expressed as a fold).

dfaaccept :: Eq st
           => Dfa st sy  -- ^ Automaton
           -> [sy]       -- ^ Input symbols
           -> Bool
dfaaccept (Dfa v q s z delta) simb   = (foldl delta s simb) `elem` z



-----------------------------------------------------------------------------
-- * Printing

-- | Print a 'Dfa' as a Haskell function.

instance (Show st, Show sy) => Show (Dfa st sy) where
      showsPrec p (Dfa v q s z delta) =
                showString ("dfa = Dfa v q s z delta") .
                showString ("\n  where \n\t v = ") .
                showList v .
                showString ("\n\t q = ") .
                showList q .
                showString ("\n\t s = ") .
                shows s .
                showString ("\n\t z = ") .
                showList z .
                showString ("\n\t -- delta :: st -> sy -> st \n") .
                showDfaDelta q v delta

-- | Helper function to show the transition function of a 'Dfa'.

showDfaDelta :: (Show st, Show sy)
             => [st] -> [sy] -> (st -> sy -> st) -> [Char] -> [Char]

showDfaDelta q v d = foldr (.) (showChar '\n') f
  where
    f     = zipWith3 showF m n q'
    (m,n) = unzip l
    q'    = map (uncurry d) l
    l     = [(a,b) | a <- q , b <- v]

    showF st sy st' = showString ("\t delta ") .
                      shows st .
                      showChar (' ') .
                      shows sy .
                      showString (" = ") .
                      shows st' .
                      showChar ('\n')


-- | Write a 'Dfa' to a Haskell module in a file.

dfaIO :: (Show st , Show sy)
      => (Dfa st sy)   -- ^ Automaton
      -> String        -- ^ Haskell module name
      -> IO ()
dfaIO afd modulename =
          writeFile (modulename ++ ".hs")
                    ("module " ++ modulename ++ " where\n\nimport Dfa\n\n"
                                 ++ (show afd))


-----------------------------------------------------------------------------
-- * Transitions


-- | Compute the labels with the same (giving) origin and destination states

transitionsFromTo :: Eq st
                  => (st -> sy -> st)      -- ^ Transition function
                  -> [sy]                  -- ^ Vocabulary
                  -> st                    -- ^ Origin
                  -> st                    -- ^ Destination
                  -> [sy]                  -- ^ Labels
transitionsFromTo delta vs o d = [ v
                                 | v <- vs
                                 , delta o v == d
                                 ]


-- | Compute the destination states giving the origin state

destinationsFrom :: (st -> sy -> st)       -- ^ Tansition Function
                 -> [sy]                   -- ^ Vocabulary
                 -> st                     -- ^ Origin
                 -> [st]                   -- ^ Destination States
destinationsFrom delta vs o = [ delta o v | v <- vs ]


-- | Compute the states that can be reached from a state
--   according to a given transition function and vocabulary

reachedStatesFrom :: (Eq st, Ord st)
                  => (st -> sy -> st)       -- ^ Transition function
                  -> [sy]                   -- ^ Vocabulary
                  -> st                     -- ^ Origin
                  -> [st]                   -- ^ Reached states

reachedStatesFrom d v origin = origin : qs
  where qs      = limit stPath' (destinationsFrom d v origin)
        stPath' = stPath d v


stPath  :: Ord st => (st -> sy -> st) -> [sy] -> [st] -> [st]
stPath d v sts = sort $ nub $
                 (sts ++ (concat $ map (destinationsFrom d v) sts))



-----------------------------------------------------------------------------
-- * Transformation

-- | Produce the transition table of a given 'Dfa'.
--   Given a 'Dfa', it returns a list of triples of the form
-- @
--                 (Origin,Symbol,Destination)
-- @
-- defining all the transitions of the 'Dfa'.
--

transitionTableDfa :: (Ord st, Ord sy)
                   => Dfa st sy          -- ^ Automaton
                   -> [(st,sy,st)]       -- ^ Transition table
transitionTableDfa (Dfa v q s z delta) = sort [ ( aq , av , delta aq av)
                                              | aq <- q
                                              , av <- v
                                              ]


-- | Produce the transition table of a given 'Dfa'. 
--   Given a 'Dfa', it returns a list of triples of the form
-- @
--                 (Origin,[Destination])
-- @
-- defining all the transitions of the 'Dfa'.
--

transitionTableDfa' :: (Ord st, Ord sy)
                    => Dfa st sy          -- ^ Automaton
                    -> [(st,[st])]        -- ^ Transition table

transitionTableDfa' (Dfa v q s z delta) = sort [ ( aq , map (delta aq) v )
                                               | aq <- q
                                               ]



-- | Reconstruct a 'Dfa' from a transition table.
--   Given an automaton expressed by a transition table
--   (ie a list of triples of the form
-- @
--                    (Origin,Symbol,Destination)
-- @
--   it constructs a 'Dfa'. The other elements of the
--   input tuple are the vocabulary, a set of
--   states, an initial state, and a set of
--   final states.

ttDfa2Dfa :: (Eq st, Eq sy)
          => ([sy],[st],st,[st],[(st,sy,st)])  -- ^ Tuple-based Automaton
          -> Dfa st sy                         -- ^ Automaton
ttDfa2Dfa (vs,qs,s,z,ld) = Dfa vs qs s z d
  where d st sy =  lookUptt st sy ld

        lookUptt q v ((a,b,c) : [])                    = c
        lookUptt q v ((a,b,c) : xs) | q == a && v == b = c
                                    | otherwise        = lookUptt  q v xs


--
-- Beautify the Automata States
--

beautifyDfaWithSyncSt :: Eq st
                      => Dfa [st] sy    -- ^ Original Automaton
                      -> Dfa [Int] sy   -- ^ Beautified Automaton (states as integers)
beautifyDfaWithSyncSt (Dfa v q s z delta) = (Dfa v q' s' z' delta')
  where qaux = (giveNumber q 1) ++ [([],[])]
        q'   = map snd qaux
        s'   = lookupSt s qaux
        z'   = getNewFinalSt z qaux
        delta' st' sy' = lookupSt (delta (lookupNewSt st' qaux) sy') qaux

lookupSt :: Eq st => st -> [(st,[Int])] -> [Int]
lookupSt s (h:t) | fst h == s = snd h
                 | otherwise  = lookupSt s t

lookupNewSt :: [Int] -> [(st,[Int])] -> st
lookupNewSt s (h:t) | snd h == s = fst h
                    | otherwise  = lookupNewSt s t

getNewFinalSt :: Eq st => [st] -> [(st,[Int])] -> [[Int]]
getNewFinalSt [] qaux = []
getNewFinalSt (h:t) qaux = (lookupSt h qaux) : getNewFinalSt t qaux

giveNumber :: Eq st
           => [[st]]
           -> Int             -- ^ Initial number
           -> [([st],[Int])]  -- ^ ??
giveNumber []    i = []
giveNumber (h:t) i | h == []   = giveNumber t i
                   | otherwise = (h,[i]) : giveNumber t (i+1)


-- | Type of Table-based Deterministic Finite Automata.
--                       v1 ,  v2 ,  ... , vn   vocabulary
--            [ ( o1 , [ d  ,  d  ,  ... , d ]) 
--            , ( o2 , [ d   , d  ,  ... , d ])
--            ... ]

type TableDfa st = [(st, [st])]


-- | Compute all states in a transtion table defining a Dfa
--   

ttAllSts :: TableDfa st -> [st]
ttAllSts = map fst

ttDestinations :: TableDfa st -> [[st]]
ttDestinations  = map snd

-- | Compute all destinations states in a (transition) table
--   It considers all states in the second element of the pair: the destion
--   states.
--   Usefull when the transition table is being constructed.

ttAllDestSts :: Eq st
            => TableDfa st   -- ^ Transition Table (maybe not totally defined)
            -> [st]          -- ^ set of states in the table
ttAllDestSts = nub . concat . ttDestinations


-- | 'Dfa' to a Transition Table-based 'Dfa'
--

dfa2tdfa :: (Eq st, Ord sy)
         => Dfa st sy              -- ^ Automaton
         -> TableDfa st            -- ^ Transition table
dfa2tdfa (Dfa v q s z delta) = limit (dfa2tdfaStep delta v') tbFstRow
  where v'       = sort v
        tbFstRow = newRows delta [s] v'

-- | Add rows to a table-based Dfa, given a Dfa transition function
--   and its vocabulary.
dfa2tdfaStep :: Eq st
             => (st -> sy -> st)  -- ^ Transition function
             -> [sy]              -- ^ Vocabulary
             -> TableDfa st       -- ^ Table-based Dfa
             -> TableDfa st       -- ^ Table-based Dfa with additional rows.
dfa2tdfaStep delta alfabet tb = tb `union` (newRows delta newSts alfabet)
  where newSts = (ttAllDestSts tb) <-> (ttAllSts tb)


newRows :: (st -> sy -> st) -> [st] -> [sy] -> TableDfa st
newRows delta qs alfabet = map (\ st -> (st, newRow delta st alfabet)) qs

newRow :: (st -> sy -> st) -> st -> [sy] -> [st]
newRow delta st alfabet = map (delta st) alfabet


-- | Complement of a 'Dfa'


complementDfa :: Eq st
              => Dfa st sy               -- ^ Original 'Dfa'
              -> Dfa st sy               -- ^ Resulting 'Dfa'
complementDfa (Dfa v qs s z delta) = Dfa v qs s (qs <-> z) delta
  


-- | Renames a 'Dfa'.
-- It renames a DFA in such a way that the renaming of two isomorphic DFA
-- returns the same DFA.
-- It is the basis for the equivalence test for minimized DFA.
--
renameDfa :: (Ord st, Ord sy)
          => Dfa st sy      -- ^ Automaton
          -> Int            -- ^ Initial state ID
          -> Dfa Int sy     -- ^ Renamed automaton
renameDfa dfa@(Dfa v q s z delta) istid = Dfa v' q' s' z' delta'
  where v'     = sort v
        q'     = sort $ map snd newSts
        tb     = dfa2tdfa dfa
        s'     = istid
        newSts = newStsOfTable tb s'
        z'     = sort $ map snd (filter (\(a,b) -> a `elem` z) newSts)
        delta' newSt sy = lookupNewSts delta newSt sy newSts

--
-- Giving new names to the Dfa states
--

newStsOfTable :: Eq st => TableDfa st -> Int -> [(st,Int)]
newStsOfTable tb ini = newStsOfTableAux tb [(fst $ head tb,ini)]

newStsOfTableAux :: Eq a => [(b,[a])] -> [(a,Int)] -> [(a,Int)]
newStsOfTableAux []           newSt = newSt
newStsOfTableAux ((st,sts):t) newSt = newSt''
    where newSt' = procrhsSts sts newSt
          newSt'' =  newStsOfTableAux t newSt'

procrhsSts :: Eq a => [a] -> [(a,Int)] -> [(a,Int)]
procrhsSts []       newSt = newSt
procrhsSts (st:sts) newSt
     | st `elem` (map fst newSt) = procrhsSts sts newSt
     | otherwise = newSt'
        where newSt' = procrhsSts sts ((st,( snd $ head newSt) + 1) : newSt)

lookupNewSts delta newSt sy newSts = getNewSt newOldSt newSts
  where newOldSt = delta (getOldSt newSt newSts) sy

getNewSt oldSt newSts = snd $ head (filter (\(a,b) -> a == oldSt) newSts)
getOldSt newSt newSts = fst $ head (filter (\(a,b) -> b == newSt) newSts)



-- | Beautify a 'Dfa' by assigning (natural) numbers to states.

beautifyDfa :: (Ord st, Ord sy) => Dfa st sy -> Dfa Int sy
beautifyDfa dfa = renameDfa dfa 1




-----------------------------------------------------------------------------
-- * Properties of 'Dfa'



-- | Compute the sync states of a 'Dfa'
--

dfasyncstates :: Eq st
              => Dfa st sy      -- ^ Automaton
              -> [st]           -- ^ Dead states
dfasyncstates  (Dfa v q _ z d) = filter (isStSync d v z) q


-- | Compute the dead states of a 'Dfa'
--

dfadeadstates :: Ord st
              => Dfa st sy      -- ^ Automaton
              -> [st]           -- ^ Dead states
dfadeadstates (Dfa v qs s z d) = filter (isStDead d v z) qs


-- | Compute the size of a deterministic finite automaton.
--   The size of an automaton is the number of its states.

sizeDfa :: Dfa st sy -> Int
sizeDfa (Dfa _ q _ _ _) = length q


-- | Compute the number of states and transtions of a 'Dfa'
--  

nodesAndEdgesDfa ::  (Eq st , Ord st , Ord sy)
                 => Dfa st sy          -- ^ Automaton
                 -> (Int , Int)        -- ^ (#States, #Transitions)
nodesAndEdgesDfa dfa@(Dfa _ q _ _ _) = (length q , length tt)
    where tt      = transitionTableDfa dfa


-- | Compute the number of states and transtions of a 'Dfa'
--   It does not consider nodes (nor transitions to) dead states nor
--   sync states.


nodesAndEdgesNoSyncDfa :: (Eq st , Ord st , Ord sy)
                       => Dfa st sy          -- ^ Automaton
                       -> (Int , Int)        -- ^ (#States, #Transitions)
nodesAndEdgesNoSyncDfa dfa@(Dfa _ q _ _ _) = (length states , length tt')
    where tt      = transitionTableDfa dfa
          syncSts = dfasyncstates dfa
          deadSts = dfadeadstates dfa
          states  = filter (\ st -> not  ((st `elem` syncSts) ||
                                          (st `elem` deadSts))) q
          tt'     = filter (\ (_ , _ , d) ->
                      not (d `elem` syncSts) || (d `elem` deadSts)) tt


-- | Compute the cyclomatic complexity of a 'Dfa'
--   The Cyclomatic Complexity (CC) is given by
--         cc = N - E + 2 * P
--      where N is the number of nodes
--            E is the number of edges
--            P is tne number of connected components


cyclomaticDfa :: (Ord st , Ord sy)
              => Dfa st sy -> Int
cyclomaticDfa dfa = e - n + 2 * p
  where (n , e) =  nodesAndEdgesNoSyncDfa dfa
        p = 1

-----------------------------------------------------------------------------
-- * Properties of States


-- | Checks whether a state is dead or not.
--
--   One state is dead when it is not possible to reach a final state from it.
--   (probably we should consider that it has to be reachable from the
--    initial state, as well)

isStDead :: Ord st
         => (st -> sy -> st)        -- ^ Transition Function
         -> [sy]                    -- ^ Vocabulary
         -> [st]                    -- ^ Set of Final States
         -> st                      -- ^ State
         -> Bool

isStDead d v z st = reachedStatesFrom d v st `intersect` z == []


-- | Checks whether a state is a sync state or not
--
--   A sync state is a state that has transitions to itself for all
--   symbols of the vocabulary ans it is not final/accepting state.


isStSync :: Eq st
         => (st -> sy -> st)        -- ^ Transition Function
         -> [sy]                    -- ^ Vocabulary
         -> [st]                    -- ^ Set of Final States
         -> st                      -- ^ State
         -> Bool
isStSync d vs z st = (not (st `elem` z))  && (and qs)
  where qs = [ st == dfawalk d st [v]
             | v <- vs
             ]

-- | Compute the number of incoming arrows for a given state
--

numberIncomingArrows :: Eq st
                     => (st -> sy -> st)       -- ^ Transition Function
                     -> [sy]                   -- ^ Vocabulary
                     -> [st]                   -- ^ Set of States
                     -> st                     -- ^ Destination
                     -> Int                    -- ^ Number of Arrows
numberIncomingArrows d vs qs dest = length  [ q
                                            | v <- vs
                                            , q <- qs
                                            , d q v == dest
                                            ]

-- | Compute the number of outgoing arrows for a given state


numberOutgoingArrows :: (st -> sy -> st)       -- ^ Transition Function
                     -> [sy]                   -- ^ Vocabulary
                     -> st                     -- ^ Origin
                     -> Int                    -- ^ Number of Arrows

numberOutgoingArrows d v o = length $ destinationsFrom d v o
