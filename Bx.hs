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
 - tenho a delta_ndfa e a delta_dfa
 - pego na cabeça do ndfa e calculo o walk, procuro 

 Case:
  normal 0 ambas as delta transitions tables estão vazias -- aplico o bigul aos estados, vocabulário e estados finais
  normal 1 [ndfawalk destinos = view ] [ndfawalk dest length = 1] -- aplico um bigul
  normal 2 [ndfawalk destinos = view ] [ndfawalk dest length > 1] -- primeiro tenho que trazer para a cabeça todas as transiçoes rearranjar a source e aplicar outro bigul
  adaptive 1 source_empty view not -- create on the source the transitions
  adaptive 2 source view empty -- return a empty source

Ndfa
Vocabulary:     'b', 'a'
States:         b2, a1, a0
Start state(s): _0
Final states:   _0, a0, a1, b2
Transitions:    (_0,'a') -> a1
                (_0,'a') -> a2
                (_0,'b') -> b2
                (b2,'b') -> b2
                (a1,'b') -> b2
                (a0,'a') -> a1
                (a0,'b') -> b2

Transitions:    (a2,'b') -> [b3]
                (b3,'b') -> [b3]
                (_0,'b') -> [b3]
                (_0,'a') -> [a2]



                (a1,'a') -> [a2]
                (a1,'b') -> [b3]

4

Dfa
Vocabulary:     'b', 'a', 'c'
States:         [a2], [a1,a2], [b3], [_0]
Start state(s): [_0]
Final states:   [a2], [a1,a2], [b3], [_0]

Transitions:    ([a2],'b') -> [b3]:
                ([b3],'b') -> [b3]:
                ([_0],'b') -> [b3]:
                ([_0],'a') -> [a2]

-}

putNdfa' :: Eq st => NdfaT st sy -> DfaT [st] sy -> NdfaT st sy
putNdfa' (NdfaT v1 q1 s1 z1 []) (DfaT v2 q2 s2 z2 []) = (NdfaT v q s z [])
    where v = v2
          q = nub $ concat q2
          s = s1
          z = f z1 (concat z2)
          f [] cz2 = cz2
          f (h:t) cz2 = if h `elem`cz2 then h:(f t (filter (/= h) cz2))
                        else f t cz2
putNdfa' (NdfaT v1 q1 s1 z1 d) dfa@(DfaT v2 q2 s2 z2 []) = putNdfa' (NdfaT v1 q1 s1 z1 []) dfa
putNdfa' (NdfaT v1 q1 s1 z1 []) dfa@(DfaT v2 q2 s2 z2 (d:ds)) = putNdfa' (NdfaT v1 q1 s1 z1 daux) dfa
    where daux = rearrangeS d
          -- mudar aqui no primeiro caso também temos que ter cuidado que ele não vá por outra
          -- rearrangeS (([o],sy),dsts) = [((o,sy),dsts)]
          rearrangeS ((os,sy),dsts)  = case [((o,sy),dsts) | o <- os \\ (concat $ delete os q2)] of
                                       [] -> error "View not consistent with source"
                                       x  -> x
--putNdfa' (NdfaT v1 q1 s1 z1 d1) @dfa(DfaT v2 q2 s2 z2 d2) = putNdfa' (NdfaT v1 q1 s1 z1 d1') @dfa(DfaT v2 q2 s2 z2 [])
--    where d1' = reflectS d1 d2 0


putNdfa :: NdfaT (Indexed Char) Char 
        -> DfaT [Indexed Char] Char
        -> Either String (NdfaT (Indexed Char) Char)
putNdfa ndfa@(NdfaT v1 q1 s1 z1 delta1) dfa@(DfaT v2 q2 s2 z2 delta2) = 
    if wellBuilt dfa then Right (putNdfa' ndfa dfa)
    else Left "View is not correct"


