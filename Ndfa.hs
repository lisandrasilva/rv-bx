module Ndfa where

import Dfa

{- NDFA where the transitions are defined as a function -}
data NdfaF st sy = NdfaF [ sy ]                      -- Vocabulary
                         [ st ]                      -- States
                         [ st ]                      -- Start states
                         [ st ]                      -- Final states
                         (st -> sy -> [ st ] )       -- Transition function


{- NDFA where the transitions are defined as a table -}
data Ndfa st sy = Ndfa [ sy ]               -- Vocabulary
                       [ st ]               -- States
                       [ st ]               -- Start states
                       [ st ]               -- Final states
                       [((st,sy),st)]       -- Transition table

{- Converts all NDFA into the tabulated version -}
class NDFA t where
  injNDFA :: t states voc -> Ndfa states voc

instance NDFA Ndfa where
  injNDFA = id

instance NDFA NdfaF where
  injNDFA (NdfaF voc states starts finals delta) 
    = Ndfa voc states starts finals delta'
      where delta' = [((o,s),d) | ((o,s),ds) <- (tabulate delta states voc)
                                , d <- ds]


{- Dfa can be made into Ndfa -}
toNonDet :: DFA t => t states voc -> Ndfa states voc
toNonDet a = let (Dfa v s i f table) = injDFA a
             in Ndfa v s [i] f table

instance NDFA DfaF where 
   injNDFA = toNonDet . injDFA 

instance NDFA Dfa where 
   injNDFA = toNonDet . injDFA 


{- Reverts a transition table -}
revNTab :: (Eq st, Eq sy) => [((st,sy),[st])] -> [((st,sy),[st])]
revNTab tab = grouping [((z',y),x) | ((x,y),z) <- tab, z' <- z]

{- Converts a relation into a function -}
grouping :: (Eq st, Eq sy) => [((st,sy),st)] -> [((st,sy),[st])]
grouping [] = []
grouping (((a,b),c):t) = let (x,t') = remove (a,b) t
                      in ((a,b),c:x):grouping t

-- Ex: remove 1 [(1,3),(2,3),(1,4),(2,4)] = ([3,4],[(2,3),(2,4)])
remove :: Eq a => a -> [(a,b)] -> ([b],[(a,b)])
remove _ [] = ([],[])
remove s' ((s,d):t) | s == s' = ((d:x'), t')
                    | otherwise = (x', (s,d):t')
    where (x',t') = remove s' t

{- Reverts an automaton -}
revAut :: (NDFA t, Eq st, Eq sy)=> t st sy -> Ndfa st sy
revAut a = 
     let (Ndfa voc stats starts final delta) = injNDFA a
         delta' = [((o,s),d) | ((d,s),o) <- delta]
     in Ndfa voc stats final starts delta'


{- Renaming the states -}
normStates :: (NDFA t, Eq st, Eq sy, Ord st) => t st sy -> Ndfa Int sy
normStates a = 
     let (Ndfa voc stats starts final delta) = injNDFA a
         stats' = [1..length stats]
         conversion = zip stats stats'
         convert s = let (Just s') = lookup s conversion
                     in s'
         final' = map convert final
         starts' = map convert starts
         delta' = [((convert x,y), convert z) | ((x,y),z) <- delta]  
     in Ndfa voc stats' starts' final' delta'


{- Computes the fix point of a function -}
limit :: Eq a => (a -> a) -> a -> a
limit f s | s == next = s
          | otherwise = limit f next
            where next = f s

