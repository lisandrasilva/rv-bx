module Ndfa where

import System.Process
import Data.List
import Dfa

{- NDFA where the transitions are defined as a function -}
data NdfaF st sy = NdfaF [ sy ]                      -- Vocabulary
                         [ st ]                      -- States
                         [ st ]                      -- Start states
                         [ st ]                      -- Final states
                         (st -> sy -> [ st ] )       -- Transition function


{- NDFA where the transitions are defined as a table -}
data NdfaT st sy = NdfaT [ sy ]                   -- Vocabulary
                         [ st ]                   -- States
                         [ st ]                   -- Start states
                         [ st ]                   -- Final states
                         [((st,sy),[ st ])]       -- Transition table


{- Converts all NDFA into the tabulated version -}
class NDFA t where
  injNDFA :: t states voc -> NdfaT states voc

instance NDFA NdfaT where
  injNDFA = id

instance NDFA NdfaF where
  injNDFA (NdfaF voc states starts finals delta) 
    = NdfaT voc states starts finals (tabulate delta states voc)


{- Dfa can be made into Ndfa -}
toNonDet :: DFA t => t states voc -> NdfaT states voc
toNonDet a = let (DfaT v s i f table) = injDFA a
             in NdfaT v s [i] f [((x,y),[z]) | ((x,y),z) <- table]

instance NDFA DfaF where 
   injNDFA = toNonDet . injDFA 

instance NDFA DfaT where 
   injNDFA = toNonDet . injDFA 


{- Reverts a transition table -}
revNTab :: (Eq st, Eq sy) => [((st,sy),[st])] -> [((st,sy),[st])]
revNTab tab = grouping [((z',y),x) | ((x,y),z) <- tab, z' <- z]
    where -- Converts a relation into a function
          -- grouping :: (Eq a, Eq b) => [((st,sy),st)] -> [((st,sy),[st])]
          grouping [] = []
          grouping (((a,b),c):t) = let (x,t') = remove (a,b) t
                                in ((a,b),c:x):grouping t
          -- remove :: Eq a => a -> [(a,b)] -> ([b],[(a,b)])
          -- Ex: remove 1 [(1,3),(2,3),(1,4),(2,4)] = ([3,4],[(2,3),(2,4)])
          remove _ [] = ([],[])
          remove s' ((s,d):t) | s == s' = ((d:x'), t')
                              | otherwise = (x', (s,d):t')
              where (x',t') = remove s' t

{- Reverts an automaton -}
revAut :: (NDFA t, Eq st, Eq sy)=> t st sy -> NdfaT st sy
revAut a = 
     let (NdfaT voc stats starts final delta) = injNDFA a
     in NdfaT voc stats final starts (revNTab delta)


{- Renaming the states -}
normStates :: (NDFA t, Eq st, Eq sy, Ord st) => t st sy -> NdfaT Int sy
normStates a = 
     let (NdfaT voc stats starts final delta) = injNDFA a
         stats' = [1..length stats]
         conversion = zip stats stats'
         convert s = let (Just s') = lookup s conversion
                     in s'
         final' = map convert final
         starts' = map convert starts
         delta' = [((convert x,y), map convert z) | ((x,y),z) <- delta]  
     in NdfaT voc stats' starts' final' delta'


{- Computes the fix point of a function -}
limit :: Eq a => (a -> a) -> a -> a
limit f s | s == next = s
          | otherwise = limit f next
            where next = f s


-- Example
-- b \ ((d + i);)^* \  e
nd1 = NdfaT "bdi;e" [1,2,3,4,5] [1] [5] delta
        where delta= delta1
delta1 = [((1,'b'),[2])
         ,((2,'d'),[3])
         ,((2,'i'),[3])
         ,((2,'e'),[5])
         ,((3,';'),[4])
         ,((4,'@'),[2])
         ]

