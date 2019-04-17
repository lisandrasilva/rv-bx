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

notas - falta verificar que as transições estão no vocabulário

{-  Checks if a DFA is well built (assuming that it was originated from a glushkov Ndfa 
    with powerset construction):
      - Every transition from (o,s) is unique
      - Each node reachs a final node
      - Each node is reachable from the start node
      - Every transition with 'sy' must have as destination nodes prefixed with 'sy'
      - Initial state cannot be modified
 -}
wellBuilt :: DfaT [Indexed Char] Char -> Bool
wellBuilt (DfaT v q s z delta) = (noRepetitions $ map fst delta) && 
                                 q \\ (reachableNodes delta [s] []) == [] && 
                                 q \\ (reachableNodes atled z []) == [] &&
                                 wellEdge delta &&
                                 s == [(I '_' 0)]
    where atled = [((d,y),o) | ((o,y),d) <- delta]
          wellEdge [] = True
          wellEdge (((_,y),ds):xs) = (all (== y) $ map getSymbol ds) && 
                                     (noRepetitions $ map getIndex ds) &&
                                     wellEdge xs

{- Given the transition table and a list of states computes the list of reachable states from that list -}
reachableNodes :: Eq st => [((st, sy), st)] -> [st] -> [st] -> [st]
reachableNodes _ [] acc = acc
reachableNodes delta (n:ns) acc = reachableNodes delta x (n:acc)
    where x = ns `union` [ d | ((o,y),d) <- delta , o == n , not (d `elem` (n:acc))]


{-  Checks if for each entry in the Dfa's table transition the walk in the Ndfa's transition table from
    from the origins gives the same set, which means if a given transition in the Dfa was modified or not

getModification :: (Ord st, Eq sy) => [((st,sy), [st])] -> [(([st],sy), [st])] -> Maybe (([st], sy), [st])
getModification _ [] = Nothing
getModification delta (((os,y),ds):xs) = if (from delta os y) == (nub $ sort ds)
                                            then getModification delta xs
                                         else Just ((os,y),ds)
    where from d ls x = nub $ sort [s' | ((i,y),s) <- d, y == x, i `elem` ls, s' <- s]
-}

{-  Checks if a given entry in the Dfa's table transition was modified or not -}
viewModified :: (Ord st, Eq sy) => [((st,sy), [st])] -> (([st],sy), [st]) -> Bool
viewModified delta ((os,y),ds) = (from delta os y) == (nub $ sort ds)
    where from d ls x = nub $ sort [s' | ((i,y),s) <- d, y == x, i `elem` ls, s' <- s]


-- Preciso do s2 porque o e-closure do NDFA == s2 e preciso do z2 porque todos os estados em q2 têm que ter um caminho até z2
-- Pôr isto a retornar erro caso o automato não esteja bem contruído

{-
Ndfa
Vocabulary:     'b', 'a'
States:         b2, a1, a0
Start state(s): _0
Final states:   _0, a0, a1, b2
Transitions:    (_0,'a') -> a0
                (_0,'a') -> a1
                (_0,'b') -> b2
                (b2,'b') -> b2
                (a1,'b') -> b2
                (a0,'a') -> a1
                (a0,'b') -> b2

Dfa
Vocabulary:     'b', 'a'
States:         [a1], [a0,a1], [b2], [_0]
Start state(s): [_0]
Final states:   [a1], [a0,a1], [b2], [_0]
Transitions:    ([a1],'b') -> [b2]
                ([a0,a1],'b') -> [b2]
                ([a0,a1],'a') -> [a1]
                ([b2],'b') -> [b2]
                ([_0],'b') -> [b2]
                ([_0],'a') -> [a0,a1]


 - tenho a delta_ndfa e a delta_dfa
 - pego na cabeça do ndfa e calculo o walk, procuro 

 Case:
  normal 0 ambas as delta transitions tables estão vazias -- aplico o bigul aos estados, vocabulário e estados finais
  normal 1 [ndfawalk destinos = view ] [ndfawalk dest length = 1] -- aplico um bigul
  normal 2 [ndfawalk destinos = view ] [ndfawalk dest length > 1] -- primeiro tenho que trazer para a cabeça todas as transiçoes rearranjar a source e aplicar outro bigul
  adaptive 1 source_empty view not -- create on the source the transitions
  adaptive 2 source view empty -- return a empty source

-}

{-
putNdfa' :: (Ord st, Eq sy) => NdfaT st sy -> DfaT [st] sy -> NdfaT st sy
putNdfa' ndfa@(NdfaT v1 q1 s1 z1 delta1) dfa@(DfaT v2 q2 s2 z2 delta2) = 
    case getModification delta1 delta2 of 
      Just ((o,y),d) -> putNdfa' (ndfaAddTransitions ndfa ((o,y),d)) dfa
      Nothing -> (NdfaT v1 q1 s1 z1 delta1)


putNdfa :: (Ord st, Eq sy) => NdfaT st sy -> DfaT [st] sy -> Either String (NdfaT st sy)
putNdfa ndfa@(NdfaT v1 q1 s1 z1 delta1) dfa@(DfaT v2 q2 s2 z2 delta2) = 
    if wellBuilt dfa then Right (putNdfa' ndfa dfa)
    else Left "View is not correct"
-}


