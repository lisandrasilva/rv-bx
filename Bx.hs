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

putNdfa :: NdfaT (Indexed Char) Char 
        -> DfaT [Indexed Char] Char
        -> Either String (NdfaT (Indexed Char) Char)
putNdfa ndfa@(NdfaT v1 q1 s1 z1 delta1) dfa@(DfaT v2 q2 s2 z2 delta2) = 
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
wellBuilt :: DfaT [Indexed Char] Char -> Bool
wellBuilt (DfaT v q s z delta) = (noRepetitions $ map fst delta) && 
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


putNdfa' :: (Ord st, Eq st, Eq sy) => NdfaT st sy -> Int -> DfaT [st] sy -> NdfaT st sy
putNdfa' (NdfaT v1 q1 s1 z1 d1) n (DfaT v2 q2 s2 z2 []) = (NdfaT v q s z d)
    where v = v2
          q = nub $ concat q2
          s = s1
          z = f z1 (concat z2)
          d = take n d1
          f [] cz2 = cz2
          f (h:t) cz2 = if h `elem` cz2 then h:(f t (filter (/= h) cz2))
                        else f t cz2
putNdfa' (NdfaT v1 q1 s1 z1 delta1) n (DfaT v2 q2 s2 z2 (d2:ds2)) = 
      putNdfa' (NdfaT v1 q1 s1 z1 (rel2Func d1')) nn (DfaT v2 q2 s2 z2 ds2)
    where delta = func2Rel delta1
          (d1', nn) = if viewModified delta d2 then (sc ++ newEdges, n + length newEdges)
                      else (sc ++ c ++ e, n + length c)
          (sc,ukn) = splitAt n delta
          (c,e) = splitEdges ukn d2
          newEdges = (rearrangeS d2 q2) \\ sc

{-  Checks if a given entry in the Dfa's table transition was modified or not -}
viewModified :: (Ord st, Eq sy) => [((st,sy), st)] -> (([st],sy), [st]) -> Bool
viewModified delta ((os,y),ds) = (from delta os y) == (nub $ sort ds)

from :: (Ord st, Eq sy) => [((st,sy), st)] -> [st] -> sy -> [st]
from delta states symbol = nub $ sort [s | ((i,y),s) <- delta, y == symbol, i `elem` states]

splitEdges ::  (Eq st, Eq sy) => [((st,sy), st)] -> (([st],sy), [st]) -> ([((st,sy), st)],[((st,sy), st)])
splitEdges [] _ = ([],[])
splitEdges (edge@((o,y),d):t) ((os,symb),ds) = 
    let (c,e) = splitEdges t ((os,symb),ds)
    in if o `elem` os && y == symb && d `elem` ds then (edge:c,e)
       else (c,edge:e)

rearrangeS :: Eq st => (([st], sy), [st]) -> [[st]] -> [((st, sy), st)]
rearrangeS ((os,sy),dsts) q = case [((o,sy),dd) | o <- os \\ (concat $ delete os q), dd <- dsts] of
                             [] -> error "View not consistent with source"
                             x  -> x

