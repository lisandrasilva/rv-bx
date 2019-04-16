module Ndfa2Dfa where

import Data.List
import RegExp
import Dfa
import Ndfa

-- há que fazer uma versão (mais simples) para o caso de autómatos
-- não deterministicos sem transições epsilon (não vai precisar de
-- calcular eclosure)

ndfaG2dfa :: (NDFA t, Ord st, Eq sy) => t st sy -> DfaT [st] sy
ndfaG2dfa a = let (NdfaT voc stats starts finals delta) = injNDFA a
                  finals' = [x | x <- stats', not(null(x `intersect` finals))]
                  (stats',delta') = buildTable delta [] [] [starts]
              in DfaT voc stats' starts finals' (nub delta')

buildTable :: (Ord st ,Eq sy)=> [((st,sy),[st])] 
                        -> [[st]]
                        -> [(([st],sy),[st])]
                        -> [[st]] 
                        -> ([[st]], [(([st],sy),[st])])
buildTable d x y [] = (x,y)
buildTable delta states tab (s:ss) = buildTable delta states' tab' (ss ++ new)
     where tab'           = tab ++ newtransitions
           newtransitions = [((s,symb),from delta s symb) | symb <- outsymbols]
           states'        = s:states
           outsymbols     = [x | ((a,x),_) <- delta, a `elem` s]
           new            =  ((nub (map snd newtransitions)) \\ states') \\ ss 

-- Não posso pôr i <- ls???
from d ls x = sort [s' | ((i,y),s) <- d, y == x, i `elem` ls, s' <- s]

{-
-- Convert a non-deterministic automaton into a deterministic one
-- Rabin–Scott powerset construction
n2D :: (NDFA t, Ord st, Eq sy) => t st sy -> DfaT [st] sy
n2D a = let (NdfaT voc stats starts finals delta) = injNDFA a
            starts' = eclosureSet delta [] starts
            finals' = [x | x<-stats', not(null(x `intersect` finals))]
            (stats',delta') = buildTable delta [] [] [starts']
        in DfaT voc stats' starts' finals' (nub delta')

-- esta martelada de fazer o nub delta devia ser mais bem feita
-- garantindo que a função buildTable não produz linhas repetidas



eclosure :: Ord st => [((st,Maybe sy),[st])] -> st -> [st]
eclosure delta s = eclosureSet delta [] [s]

eclosureSet :: Ord st => [((st,Maybe sy),[st])] -> [st] -> [st] -> [st]
eclosureSet d acc [] = acc
eclosureSet d acc (h:t) = let one = (findEpsilon d h)
                              new = one \\ acc
                          in eclosureSet d (mergeUniq acc new) (new ++ t) 

findEpsilon :: Ord st => [((st,Maybe sy),[st])] -> st -> [st]
findEpsilon tab s = sort (s:[s' | ((x,Nothing),d) <- tab, x==s, s' <- d])

mergeUniq :: Ord a => [a] -> [a] -> [a]
mergeUniq [] y = y
mergeUniq x [] = x
mergeUniq (x:xs) (y:ys) | x == y = x:(mergeUniq xs ys)
                        | x < y  = x:(mergeUniq xs (y:ys))
                        | x > y  = y:(mergeUniq (x:xs) ys)

buildTable :: (Ord st ,Eq sy)=> [((st,Maybe sy),[st])] 
                        -> [[st]]
                        -> [(([st],sy),[st])]
                        -> [[st]] 
                        -> ([[st]], [(([st],sy),[st])])
buildTable d x y [] = (x,y)
buildTable delta states tab (s:ss) = buildTable delta states' tab' (ss ++ new)
     where tab'           = tab ++ newtransitions
           newtransitions = [((s,symb),eclosureSet delta [] (from delta s symb)) | symb <- outsymbols]
           states'        = s:states
           outsymbols     = [x | ((a,Just x),_) <- delta, a `elem` s]
           new            =  ((nub (map snd newtransitions)) \\ states') \\ ss 

from d ls x = sort [s' | ((i,Just y),s) <- d, y == x, i `elem` ls, s' <- s]
-}

{-

type StDfa st = [st]
type CT    st = [( StDfa st, [StDfa st])]

-- Given a delta function, an AFD state and the vocabulary gives the list of the AFD states for the other entries
oneRow :: Ord st 
       => (st -> Maybe sy -> [st]) -> StDfa st -> [sy] -> [StDfa st]
oneRow delta sts alfabet = map (\ v -> sort (ndfawalk delta sts [v])) alfabet

f5_ndfa_a1 = Ndfa ['a','b'] ['A','B','C','D'] ['A'] ['D'] f5_delta_a1

f5_delta_a1 'A' (Just 'a') = ['B','C']
f5_delta_a1 'A' Nothing    = ['D']
f5_delta_a1 'B' (Just 'a') = ['C']
f5_delta_a1 'C' (Just 'b') = ['D']
f5_delta_a1 'D' Nothing    = ['C']
f5_delta_a1  _  _          = []


alinea_1 = epsilon_closure f5_delta_a1 ['A']
-- alinea_1 = "ACD"
alinea_2 = oneRow f5_delta_a1 alinea_1 ['a','b']
-- alinea_2 = ["BC","CD"]

-- Given the delta funtion, the set of AFD states and the vocabulary fills the row for each one of the AFD states
consRows :: Ord st => (st -> Maybe sy -> [st]) -> [StDfa st] -> [sy] -> CT st
consRows delta []     v = []
consRows delta (q:qs) v = nub ((q , oneRow delta q v) : (consRows delta qs v))

alinea_3 = consRows f5_delta_a1  alinea_2 ['a','b']
-- alinea_3 = [("BC",["C","CD"]),("CD",["","CD"])]

-- Given the delta function, the vocabulary and a partially completed table gives the completed conversion table
ndfa2CtStep :: Ord st => (st -> Maybe sy -> [st]) -> [sy] -> CT st -> CT st
ndfa2CtStep delta v []          = []
ndfa2CtStep delta v ((s,ss):rs) = (s,ss):(consRows delta ss v) `union` (ndfa2CtStep delta v rs)

alinea_4 = ndfa2CtStep  f5_delta_a1  ['a','b'] [("ACD",["BC","CD"])]
-- [("ACD",["BC","CD"]),("BC",["C","CD"]),("CD",["","CD"])]

alinea_4b = ndfa2CtStep f5_delta_a1 ['a','b'] alinea_4 

alinea_4c = ndfa2CtStep f5_delta_a1 ['a','b'] alinea_4b


ndfa2ct :: Ord st => Ndfa st sy -> CT st 
ndfa2ct (Ndfa v q s z delta) = limit (ndfa2CtStep delta v) ttFstRow
  where  ttFstRow = consRows delta [epsilon_closure delta s] v

alinea_5 = ndfa2ct f5_ndfa_a1


ndfa2dfa :: (Ord st,Eq sy) => Ndfa st sy -> Dfa [st] sy
ndfa2dfa ndfa@(Ndfa v q s z delta)  = (Dfa v' q' s' z' delta') 
  where  tt = ndfa2ct ndfa 
         v' = v 
         q' = map fst tt
         s' = fst (head tt)
         z' = finalStatesDfa q' z
         delta' st sy = lookupCT st sy tt v

finalStatesDfa :: Eq st => [StDfa st] -> [st] -> [StDfa st]
finalStatesDfa []     z  = []
finalStatesDfa (q:qs) z  | (q `intersect` z /= [])  = q : finalStatesDfa qs z
                         | otherwise                = finalStatesDfa qs z


lookupCT :: (Eq st, Eq sy) => [st] -> sy -> CT st -> [sy] -> StDfa st
lookupCT st sy ct v = qs !! col
  where Just qs   = lookup st ct
        Just col  = elemIndex sy v
-}


