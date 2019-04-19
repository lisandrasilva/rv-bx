module Dfa2RegExp where

import Dfa
import Ndfa
import RegExp
import ShowAut
import RegExp2Aut
import Ndfa2Dfa

ex1,ex2,ex3 :: DfaT Int Char
ex3 = DfaT "ab"  [0,1,2]   0 [1] [((0,'a'),1),((1,'b'),2),((2,'a'),0)]
ex2 = DfaT "a"   [0]       0 [0] [((0,'a'),0)]
ex1 = DfaT "abc" [0,1,2,3] 0 [3] [((0,'a'),1),((1,'b'),2),((2,'c'),3)]

-- Kleene's algorithm to transform a given deterministic finite automaton into a regular expression.
-- https://en.wikipedia.org/wiki/Kleene%27s_algorithm
-- assumes that the (n+1) states are numbered from 0 to n and the initial state is 0
dfa2RegExp :: DfaT Int Char -> RegExp
dfa2RegExp (DfaT voc states start finals delta)
    = let n = (length states) -1
          initialR = (identity n) $ (process delta) $ emptys
          emptys = replicate (n+1) (replicate (n+1) Empty)
          process tr m = foldr (\(o,d,l) -> updateM o d (const (Literal l))) m [(o,d,l) | ((o,l),d) <- tr]
          identity n m = foldr (\i -> updateM i i (Or Epsilon)) m [0..n]
          r = loop n initialR 
      in foldr1 Or (selectRangeIndex finals (r !! start))

loop :: Int -> [[RegExp]] -> [[RegExp]]
loop n r = foldr step r [(i,j,k) | k <- [0..n], i <- [0..n], j <- [0..n]]
      where step (i,j,k) m = updateM i j ( simplify . (Or (Then (selectM i k m) (Then (Star (selectM k k m)) (selectM k j m))))) m

-- simplifies a regular expression
simplify (Or e1 e2) = case (simplify e1, simplify e2) of
                         (Empty,se2) -> se2
                         (se1,Empty) -> se1
                         (se1,se2) -> if se1 == se2 then se1 else (Or se1 se2)
simplify (Then e1 e2) = case (simplify e1, simplify e2) of
                         (Empty,se2) -> Empty
                         (se1,Empty) -> Empty
                         (Epsilon,se2) -> se2
                         (se1,Epsilon) -> se1
                         (se1,se2)-> Then se1 se2
simplify (Star e) = case simplify e of
                      Empty -> Empty
                      Epsilon -> Epsilon
                      (Or Epsilon se) -> Star se
                      (Or se Epsilon) -> Star se
                      se -> Star se
simplify e = e

-- matrix operations 

-- (update i j f m) updates matrix m by applying f to the element at position (i,j)
updateM :: Int -> Int -> (a->a) -> [[a]] -> [[a]]
updateM l c f matriz =
     let (as,b:bs) = splitAt l matriz
         (cs,d:ds) = splitAt c b
     in as ++ [cs ++ (f d):ds] ++ bs


selectM :: Int -> Int -> [[a]] -> a
selectM i j m = (m!!i)!!j


--selects the elements whose indexes are in a given set
selectRangeIndex :: [Int] -> [a] -> [a]
selectRangeIndex = selRI 0 
   where selRI i range [] = []
         selRI i range (h:t) | i `elem` range = h:(selRI (i+1) range t)
                             | otherwise      = selRI (i+1) range t
