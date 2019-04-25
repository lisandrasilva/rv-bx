module Bx where

import Data.List
import System.Process
import RegExp
import Dfa
import Ndfa
import RegExp2Aut
import AuxiliaryTypes
import Examples
import ShowAut
import Ndfa2Dfa


{- Given a NDFA as a source and a DFA as a view returns the updated source or error 
   if the view is not correct or consistent with the source
-}
putNdfa :: Ndfa (Indexed Char) Char 
        -> Dfa [Indexed Char] Char
        -> Either String (Ndfa (Indexed Char) Char)
putNdfa ndfa@(Ndfa v1 q1 s1 z1 delta1) dfa@(Dfa v2 q2 s2 z2 delta2) = 
    if wellBuilt dfa then Right (putNdfa' ndfa 0 dfa)
    else Left "View is not correct"

{-  Checks if a DFA is well built (assuming that it was originated from a glushkov Ndfa 
    with powerset construction):
      - Every transition from (o,s) is unique
      - 'q' must be the set of reachable nodes from the start node
      - Each node reachs a final node
      - For every transition with 'sy':
          - 'sy' belongs to 'v'
          - the destination nodes must be prefixed with 'sy'
      - Initial state cannot be modified
 -}
wellBuilt :: Dfa [Indexed Char] Char -> Bool
wellBuilt (Dfa v q s z delta) = (noRepetitions $ map fst delta) && 
                                 (sort $ nub q) == (sort $ nub $ (reachableNodes delta [s] [])) && 
                                 q \\ (reachableNodes atled z []) == [] &&
                                 wellEdge delta &&
                                 s == [(I '_' 0)] &&
                                 z \\ q == []
    where atled = [((d,y),o) | ((o,y),d) <- delta]
          wellEdge [] = True
          wellEdge (((_,y),ds):xs) = (all (== y) $ map getSymbol ds) && 
                                     (noRepetitions $ map getIndex ds) && 
                                     y `elem` v &&
                                     wellEdge xs

{- Given the transition table and a list of states computes the list of reachable states from that list -}
reachableNodes :: Eq st => [((st, sy), st)] -> [st] -> [st] -> [st]
reachableNodes _ [] acc = acc
reachableNodes delta (n:ns) acc = reachableNodes delta x (n:acc)
    where x = ns `union` [ d | ((o,y),d) <- delta , o == n , not (d `elem` (n:acc))]


{- Given a NDFA, an integer corresponding to the valid transitions in the source and a DFA
   reflects the changes in the DFA into the DFA
-}
putNdfa' :: (Ord st, Ord sy) => Ndfa st sy -> Int -> Dfa [st] sy -> Ndfa st sy
putNdfa' (Ndfa v1 q1 s1 z1 d1) n (Dfa v2 q2 s2 z2 []) = (Ndfa v q s z d)
    where v = v2
          q = nub $ concat q2
          s = s1
          z = f z1 (concat z2)
          d = sort $ take n d1
          f [] cz2 = cz2
          f (h:t) cz2 = if h `elem` cz2 then h:(f t (filter (/= h) cz2))
                        else f t cz2
putNdfa' (Ndfa v1 q1 s1 z1 delta1) n (Dfa v2 q2 s2 z2 (d2:ds2)) = 
      putNdfa' (Ndfa v1 q1 s1 z1 d1') nn (Dfa v2 q2 s2 z2 ds2)
    where (d1', nn) = if viewModified delta1 d2 then (sc ++ newEdges, n + length newEdges)
                      else (sc ++ c ++ e, n + length c)
          (sc,ukn) = splitAt n delta1
          (c,e) = splitEdges ukn d2
          newEdges = (rearrangeS d2 q2) \\ sc


{-  Checks if a given entry in the Dfa's table transition was modified or not -}
viewModified :: (Ord st, Eq sy) => [((st,sy), st)] -> (([st],sy), [st]) -> Bool
viewModified delta ((os,y),ds) = (from delta os y) /= (nub $ sort ds)

{- Given a NDFA's transition table, a list of origins and a symbol computes the list of next 
   states from origins through symbol 
-}
from :: (Ord st, Eq sy) => [((st,sy), st)] -> [st] -> sy -> [st]
from delta states symbol = nub $ sort [s | ((i,y),s) <- delta, y == symbol, i `elem` states]

{- Given a NDFA's transition table and a transition in the DFA computes the pair (b,n) where:
    - b is the list of NDFA transitions that support the DFA transition
    - n is the remaining transitions
-}
splitEdges ::  (Eq st, Eq sy) => [((st,sy), st)] -> (([st],sy), [st]) -> ([((st,sy), st)],[((st,sy), st)])
splitEdges [] _ = ([],[])
splitEdges (edge@((o,y),d):t) ((os,symb),ds) = 
    let (c,e) = splitEdges t ((os,symb),ds)
    in if o `elem` os && y == symb && d `elem` ds then (edge:c,e)
       else (c,edge:e)

{- Given a transition in the DFA computes all valid transtions in the NDFA. For that it excludes 
   from the origins nodes that also appear in other nodes, since it would create inconsistencies.
-}
rearrangeS :: Eq st => (([st], sy), [st]) -> [[st]] -> [((st, sy), st)]
rearrangeS ((os,sy),dsts) q = case [((o,sy),dd) | o <- os \\ (concat $ delete os q), dd <- dsts] of
                             [] -> error "View not consistent with source"
                             x  -> x

-- Adds a new transition no a DFA
addTransition :: (Eq st, Eq sy) => Dfa [st] sy -> (([st],sy), [st]) -> Dfa [st] sy
addTransition (Dfa v q s z d) edge@((os,symb),ds) = Dfa nv nq s z nd
    where nd = case lookup (os,symb) d of
            Just x -> error "Dfa cannot have a transition with the same symbol for the same state"
            Nothing -> edge:d
          nq = nub (q ++ [os] ++ [ds])
          nv = nub (symb:v)
