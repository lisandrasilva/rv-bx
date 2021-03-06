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
        -> Error (Ndfa (Indexed Char) Char)
putNdfa ndfa dfa = 
    case wellBuilt dfa of 
      Ok True -> Ok (putNdfaStruct ndfa dfa)
      Error m -> Error m

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
wellBuilt :: Dfa [Indexed Char] Char -> Error Bool
wellBuilt (Dfa v q s z delta) = do u <- uniqueTransitionSymbol
                                   r <- reachableFromStartNode
                                   e <- everyReachFinalNode
                                   a <- allEdgesCorrect
                                   s <- staticInitialState
                                   f <- finalStatesCorrect
                                   return (u && r && e && a && s && f)
    where atled = [((d,y),o) | ((o,y),d) <- delta]
          wellEdge [] = True
          wellEdge (((_,y),ds):xs) = (all (== y) $ map getSymbol ds) && 
                                     (noRepetitions $ map getIndex ds) && 
                                     y `elem` v &&
                                     wellEdge xs
          uniqueTransitionSymbol = if noRepetitions $ map fst delta then Ok True
                               else Error "Every transition from Node 'x' with symbol 's' must be unique"
          reachableFromStartNode = if (sort $ nub q) == (sort $ nub $ (reachableNodes delta [s] []))
                                      then Ok True
                                   else Error "Every Node 'x' must be reachable from the start node"
          everyReachFinalNode = if (q \\ (reachableNodes atled z []) == []) then Ok True
                                else Error "Every node must reach an accepting node"
          allEdgesCorrect = if wellEdge delta then Ok True
                            else Error "Transitions are not well built"
          staticInitialState = if s == [(I '_' 0)] then Ok True
                               else Error "Initial state cannot be changed"
          finalStatesCorrect = if z \\ q == [] then Ok True
                               else Error "Final states are not in set of states"



{- Given the transition table and a list of states computes the list of reachable states from that list -}
reachableNodes :: Eq st => [((st, sy), st)] -> [st] -> [st] -> [st]
reachableNodes _ [] acc = acc
reachableNodes delta (n:ns) acc = reachableNodes delta x (n:acc)
    where x = ns `union` [ d | ((o,y),d) <- delta , o == n , not (d `elem` (n:acc))]


{- Given a NDFA, an integer corresponding to the valid transitions in the source and a DFA
putNdfa' :: (Ord st, Ord sy) => Ndfa st sy -> Int -> Dfa [st] sy -> Error (Ndfa st sy)
putNdfa' (Ndfa v1 q1 s1 z1 d1) n (Dfa v2 q2 s2 z2 []) = Ok (Ndfa v q s z d)
-}
putNdfaStruct :: (Ord st, Ord sy) => Ndfa st sy  -> Dfa [st] sy -> Ndfa st sy
putNdfaStruct (Ndfa v1 q1 s1 z1 d1) (Dfa v2 q2 s2 z2 d2) = Ndfa v q s z d
    where v = v2
          q = nub $ concat q2
          s = s1
          z = f z1 (concat z2)
          d = getTable d1 (zip d2 (repeat [])) q2
          f [] cz2 = cz2
          f (h:t) cz2 = if h `elem` cz2 then h:(f t (filter (/= h) cz2))
                        else f t cz2

getTable :: (Ord st, Ord sy) => [((st,sy),st)]  -> [((([st],sy),[st]),[st])] -> [[st]] -> [((st,sy),st)]
getTable [] dfaT q = let toAdd = filter (not . null . snd) [ (((os,d),ds\\rs)) | (((os,d),ds),rs) <- dfaT]
                     in concat $ map (`rearrangeS` q) toAdd
getTable (t@((o,s),d):ts) dfaT q = 
        let relS = [ x | x <- q , o `elem` x ]
            trns = [ 1 | (((os,sym),ds),rs) <- dfaT , os `elem` relS , sym == s , d `elem` ds]
            dfaT' = map (updateListAux t) dfaT
        in if ((length relS) == (length trns)) && 
              (not $ null relS) 
           then t:getTable ts dfaT' q
           else getTable ts dfaT q
    where updateListAux ((o,s1),d) (((os,s2),ds),rs) = 
                if o `elem` os && s1 == s2 && d `elem` ds then (((os,s2),ds),d:rs)
                else (((os,s2),ds),rs)

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
rearrangeS ((os,sy),dsts) q = let min = [((o,sy),dd) | o <- os \\ (concat $ delete os q), dd <- dsts]
                              in if null min then [((o,sy),dd) | o <- os, dd <- dsts]
                                 else min 
