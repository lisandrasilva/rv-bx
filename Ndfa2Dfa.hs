module Ndfa2Dfa where
  
import Data.List
import AuxiliaryTypes
import Dfa
import Ndfa

{-  Convert a non-deterministic automaton with NO Epsilon transitions into a deterministic 
    whose states are sets of the original states 
    Rabin–Scott powerset construction
    https://en.wikipedia.org/wiki/Powerset_construction
-}
n2D :: (NDFA t, Ord st, Eq sy) => t st sy -> Dfa [st] sy
n2D a = let (Ndfa voc stats starts finals delta) = injNDFA a
            starts' = starts
            finals' = [x | x<-stats', not(null(x `intersect` finals))]
            (stats',delta') = closedTab delta [] [] [starts']
            voc' = voc
        in Dfa voc' stats' starts' finals' delta'

{-  Given the Ndfa transition table computes the transition table for the Dfa -}
closedTab :: (Ord st, Eq sy) => 
            [((st,sy),st)] 
            -> [[st]]
            -> [(([st], sy),[st])]
            -> [[st]] 
            -> ([[st]], [(([st],sy),[st])])
closedTab _ states trans [] = (states, trans)
closedTab delta states trans (xs:xss) 
  = closedTab delta (xs:states) trans' (xss `union` newStates)
        where outsymbols = rel2Func [(y,o) | ((a,y),o) <- delta, a `elem` xs] 
              newlines = map (\(x,y)->((xs,x),nub $ sort y)) outsymbols 
              trans' = newlines ++ trans
              newStates = ((map snd newlines) \\ (xs:states)) \\ xss

-- Auxiliary functions 

{-  Merges a list of sorted lists with no repetitions into a single ordered list with no repetitions -}
sortUnique :: Ord a => [[a]] -> [a]
sortUnique = foldr mergeUniq [] 

{- Merges two sorted lists with no repetitions into a single ordered list with no repetitions -}
mergeUniq :: Ord a => [a] -> [a] -> [a]
mergeUniq [] y = y
mergeUniq x [] = x
mergeUniq (x:xs) (y:ys) | x == y = x:(mergeUniq xs ys)
                        | x < y  = x:(mergeUniq xs (y:ys))
                        | x > y  = y:(mergeUniq (x:xs) ys)

{- Converts a relation a<->b into a function a->[b] -}
rel2Func :: (Eq a, Eq b) => [(a,b)] -> [(a,[b])]
rel2Func = foldr add [] 
   where add (x,y) [] = [(x,[y])]
         add (x,y) ((z,ys):t) 
             | x == z    = ((x,addNoRep y ys)):t 
             | otherwise = (z,ys):(add (x,y) t)
         addNoRep x [] = [x]
         addNoRep x (y:ys) | x==y = (y:ys)
                           | otherwise = y:(addNoRep x ys)

{- Converts a function a->[b] into a relation a<->b  -}
func2Rel :: [(a,[b])] -> [(a,b)]
func2Rel l = [(x,y) | (x,ys) <- l, y <- ys]


