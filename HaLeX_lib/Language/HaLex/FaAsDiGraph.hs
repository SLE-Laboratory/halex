-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.FaAsDiGraph
-- Copyright   :  (c) João Saraiva 2001,2002,2003,2004,2005, 2016
-- License     :  LGPL
--
-- Maintainer  :  saraiva@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- Finite Automata as Directed Graphs in GraphViz.
-- Code Included in the Lecture Notes on
--             Language Processing (with a functional flavour).
--
-----------------------------------------------------------------------------

module Language.HaLex.FaAsDiGraph (
                      ndfa2graphviz
                    , ndfa2graphviz2file
                    , dfa2graphviz
                    , dfa2graphviz2file
                    , tographviz
                    , tographviz'		    
                    , tographvizIO
		    , tographvizIO'
                    , dfa2DiGraphWithNoSyncSt
                    , dfaDiGraphWithNoSyncStIO
                    , genOneArrow
                    ) where

import Language.HaLex.RegExp
import Language.HaLex.RegExp2Fa
import Language.HaLex.Ndfa
import Language.HaLex.Dfa
import Language.HaLex.FaOperations
import Language.HaLex.Minimize


-- | Print a 'Ndfa' in GraphViz
ndfa2graphviz ndfa name = tographviz ndfa name "circle" "LR" (show . show)

-- | Print a 'Ndfa' in GraphViz in a file
ndfa2graphviz2file ndfa name =
         writeFile (name++".dot") (ndfa2graphviz ndfa  name)


-- | Print a 'Dfa' in GraphViz
dfa2graphviz dfa name =
         tographviz (dfa2ndfa dfa) name "circle" "LR" (show . show)

-- | Print a 'Dfa' in GraphViz in a file
dfa2graphviz2file dfa name = writeFile (name++".dot") (dfa2graphviz dfa  name)



-- | Print a 'Ndfa' in GraphViz/dot notation (default function)
tographviz :: (Eq sy, Show sy, Ord st, Show st)
        => Ndfa st sy            -- ^ Automaton
        -> [Char]                -- ^ Graph's name
        -> [Char]                -- ^ Node's shape
        -> [Char]                -- ^ Orientation
        -> (st -> [Char])        -- ^ Show function to print the state ids
        -> [Char]
tographviz ndfa@(Ndfa v q s z delta) name shape orientation showState =
  tographviz' ndfa name shape orientation showState show  False False


-- | Print a 'Ndfa' in GraphViz/dot notation
tographviz' :: (Eq sy, Show sy, Ord st, Show st) 
        => Ndfa st sy            -- ^ Automaton
        -> [Char]                -- ^ Graph's name
        -> [Char]                -- ^ Node's shape
        -> [Char]                -- ^ Orientation
        -> (st -> [Char])        -- ^ Show function to print the state ids
        -> (sy -> [Char])        -- ^ Show function to print the labels
        -> Bool                  -- ^ Show dead states?
        -> Bool                  -- ^ Show sync states?
        -> [Char]
tographviz' ndfa@(Ndfa v q s z delta) name shape orientation 
           showState showLabel deadSt syncSt = "digraph \"" ++ name ++ "\" {\n " 
             ++ "rankdir = " ++ orientation ++ " ;\n " 
             ++ (showElemsListPerLine (showStates q)) ++ "\n " 
             ++ (showElemsListPerLine (showInitialStates s)) ++ "\n " 
             ++ (showElemsListPerLine (showFinalStates' z))
             ++ (showElemsListPerLine (showNdfaArrows ndfa showState showLabel deadSt syncSt)) 
             ++ "node [shape=none, lavel=initialState, style = invis];\n"
             ++ (createInitialArrows (mirroredInitialStates s 1) s)
             ++ "\n}"
  where 
    showElemsListPerLine :: [String] -> String
    showElemsListPerLine []    = ""
    showElemsListPerLine (h:t) = ((showString h) "\n ") ++ (showElemsListPerLine t) 

    showStates qs = [ (showState q) ++ 
                     " [shape=" ++ shape ++" , label=" ++ (showState q) ++ " ,color=black];" 
                    | q <- qs 
                    , not (ndfaIsStDead delta v z q ) || deadSt
                    , not (ndfaIsSyncState delta v z q) || syncSt]
 
    showInitialStates ss = map showInitialState ss

    showInitialState  s = (showState s) 
                          ++ " [shape=" ++ shape ++ " , label= " ++ (showState s) 
                          ++ " , color=green];\n " 

--    showFinalStates' :: Show a => [a] -> [String]
    showFinalStates' zs = [ (showState z) ++ " [shape=double" ++ shape ++" , color=red];" 
                          | z <- zs ]



-- Creating the incoming arrows for the initial states
-- (for each state we create an invisible node and a arrow connecting to the initial one)

    mirroredInitialStates [] _     = []
    mirroredInitialStates (x:xs) n = ("\"_newState_" ++ (show n) ++ "\"") : 
                                     mirroredInitialStates xs (n+1)

    createInitialArrows [] []         = " "
    createInitialArrows (x:xs) (y:ys) = x ++ " -> " ++ (showState y) ++ 
                                        " [color = green];\n" ++ 
                                        createInitialArrows xs ys




-- | Show the arrows between nodes (states) induced by the 'Ndfa' transitions.
showNdfaArrows :: (Ord st,Show st,Show sy,Eq sy)
               => Ndfa st sy         -- ^ Automaton
	       -> (st -> String)     -- ^ Show function to print the state ids
	       -> (sy -> String)     -- ^ Show function to print the labels
	       -> Bool               -- ^ Show dead states?
	       -> Bool               -- ^ Show sync states?
	       -> [String]
showNdfaArrows ndfa@(Ndfa v q s z delta) showState showLabel deadSt syncSt = 
     map (\ (o,l,d) -> if deadSt then  if (not syncSt) && (ndfaIsSyncState delta v z o) || (ndfaIsSyncState delta v z d)  then ""
                                          else  genOneArrow (showState o) (showLabels showLabel l) (showState d) 
                          else if ((ndfaIsStDead delta v z o) || (ndfaIsStDead delta v z d)) then ""
                                  else genOneArrow (showState o) (showLabels showLabel l) (showState d))
         ((groupMoves . transitionTableNdfa) ndfa)




-- | Group labels with same origin and destination.
-- Given the Transition Table of a Ndfa it groups the transtions with
-- the same origin and destination into a single transition, whose transtion
-- is the list of labels of the original transtions.
--

groupMoves []           = []
groupMoves ((o,l,d):rs) = res
  where (l',rs') = groupMoves' (o,l,d) ((o,l,d):rs)
        res      = (o,l',d) : groupMoves rs'


groupMoves' :: (Eq st, Eq sy) => (st,Maybe sy,st) -> [(st,Maybe sy,st)]
            -> ([Maybe sy],[(st,Maybe sy,st)])
groupMoves' _ []  = ([],[])
groupMoves' (o,l,d) ((o',l',d'):rs)
             | o==o' && d==d' = (new_label,rs')
             | otherwise      = (l'', (o',l',d') : rs')
       where (l'',rs') = groupMoves' (o,l,d) rs
             new_label = if l'' == [] then [l']
                                      else l' : l''

{-
showNdfaArrows :: (Ord st,Show st,Show sy) => Ndfa st sy -> [String]
showNdfaArrows (Ndfa vs qs s z delta) = [ genOneArrow (show q) (show v) (show r)
                                        | q <- qs , v <- vs
                                        , r <- delta q (Just v)
                                        , not (ndfaIsStDead delta vs z r )
                                        , not (ndfaIsStDead delta vs z q )
                                        ] ++
                                        [ genOneArrow (show q) "Epsilon" (show r)
                                        | q <- qs
                                        , r <- delta q Nothing
                                        , not (ndfaIsStDead delta vs z r )
                                        , not (ndfaIsStDead delta vs z q )
                                        ]
-}



showLabels :: (st -> String) -> [Maybe st] -> String
showLabels _ []             = ""
showLabels showLabel (x:xs) = 
     case x of
         Just a -> (showLabel a) ++ if (showLabels showLabel xs == "") then ""
                               else ("," ++ showLabels showLabel xs)
         Nothing -> "Epsilon" ++ if (showLabels showLabel xs == "") then ""
                                 else ("," ++ showLabels showLabel xs)


genOneArrow :: String -> String -> String -> String
genOneArrow orin label dest = orin ++ " -> " ++ dest
                              ++ " [label = " ++ (show label) ++ "];"


-- | Save a 'Ndfa' in a GraphViz/dot file (default function)
tographvizIO :: (Eq sy, Show sy, Ord st , Show st) 
        => Ndfa st sy            -- ^ Automaton
        -> [Char]                -- ^ Graph's name
        -> [Char]                -- ^ Node's shape
        -> [Char]                -- ^ Orientation
        -> (st -> [Char])        -- ^ Show function to print the state ids
        -> IO()
tographvizIO ndfa name shape orientation showState =
   writeFile (name++".dot") (tographviz ndfa name shape orientation showState)


-- | Save a 'Ndfa' in a GraphViz/dot file
tographvizIO' :: (Eq sy, Show sy, Ord st , Show st) 
        => Ndfa st sy            -- ^ Automaton
        -> [Char]                -- ^ Graph's name
        -> [Char]                -- ^ Node's shape
        -> [Char]                -- ^ Orientation
        -> (st -> [Char])        -- ^ Show function to print the state ids
        -> (sy -> [Char])        -- ^ Show function to print the labels
        -> Bool                  -- ^ Show dead states?
        -> Bool                  -- ^ Show sync states?
        -> IO()

tographvizIO' ndfa name shape orient showSt showLb deadSt syncSt =
   writeFile (name++".dot")
             (tographviz' ndfa name shape orient showSt showLb deadSt syncSt)


dfa2DiGraphWithNoSyncSt dfa name = dfa2graphviz dfa name

dfa2DiGraphIO dfa name fn = writeFile (fn++".gph") (dfa2graphviz dfa  name )

dfaDiGraphWithNoSyncStIO dfa name fn = writeFile fn (dfa2graphviz dfa  name)

-- dfa2DiGraphIO'' :: (Show sy, Ord sy , Eq st) => Dfa st sy -> [Char] -> IO ()
dfa2DiGraphIO'' dfa name = dfa2DiGraphIO (beautifyDfa dfa) name name


