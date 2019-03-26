module Ndfa where

import Data.List
import Dfa                     

data Ndfa st sy = Ndfa  [ sy ]  -- Vocabulary
                        [ st ]  -- Finite set of states
                        [ st ]  -- The set of start states
                        [ st ]  -- The set of final states
                        (st -> Maybe sy -> [ st ] )   -- The transition function

instance (Show st, Show sy) => Show (Ndfa st sy) where 
  show (Ndfa v q s z delta) = "Vocabulary: " ++ (show v) ++ 
                             "\nStates: " ++ (show q) ++ 
                             "\nInit: " ++ (show s) ++
                             "\nFinal: " ++ (show z) ++
                             "\nDelta: " ++ show (showDelta delta q v)

showDeltaNdfa :: (st -> Maybe sy -> [st]) -> [st] -> [sy] -> [(st,sy,st)]
showDeltaNdfa d q v = [(x, y, d x (Just y) | x <- q, y <- v ] ++ [(x,'@', d x Nothing) | x <- q]

delta :: Ndfa st sy -> (st -> Maybe sy -> [ st ])
delta (Ndfa _ _ _ _ x) = x

-- b \ ((d + i);)^* \  e
nd1 = Ndfa "bdi;e" [1,2,3,4,5,6] [1] [5] delta
        where delta 1 (Just 'b') = [2]
              delta 2 (Just 'd') = [3]
              delta 2 (Just 'i') = [3]
              delta 2 (Just 'e') = [5]
              delta 3 (Just ';') = [4]
              delta 4 Nothing = [2]
              delta _ _ = [6]

ndfa1 = Ndfa "abc" [1,2,3,4,5,6] [1] [5,2] delta
        where delta 1 (Just 'a') = [2,3]
              delta 1 (Just 'b') = [4]
              delta 2 (Just 'b') = [3,5]
              delta 2 (Just 'c') = [2]
              delta 2 Nothing = [1]
              delta 3 Nothing = [4,5]
              delta 4 (Just 'a') = [4,5]
              delta _ _ = [6]

ndfa2 = Ndfa "abc" ['A','B','C','D','E','F'] ['A','B'] ['D'] delta
        where delta 'A' (Just 'a') = ['B']
              delta 'A' Nothing = ['A','D']
              delta 'B' (Just 'b') = ['C']
              delta 'B' Nothing = ['D']
              delta 'C' Nothing = ['B','D']
              delta 'D' (Just 'c') = ['D']
              delta 'D' Nothing = ['D']
              delta _ _ = ['F']


delta' delta []       sy = []
delta' delta (st:sts) sy = (delta st sy) `union` (delta' delta sts sy)

limit :: Eq a => (a -> a) -> a -> a
limit f s | s == next = s
          | otherwise = limit f next
            where next = f s

epsilon_closure :: Ord st => (st -> Maybe sy -> [st]) -> [st] -> [st]
epsilon_closure delta = limit f
  where f sts = sort (sts `union` (delta' delta sts Nothing))




ndfaaccept :: Ord st => Ndfa st sy -> [sy] -> Bool
ndfaaccept (Ndfa v q s z delta) sy =
  (ndfawalk delta (epsilon_closure delta s) sy) `intersect` z /= []

ndfawalk :: Ord t => (t -> Maybe sy -> [t]) -> [t] -> [sy] -> [t]
ndfawalk delta sts [] = sts
ndfawalk delta sts (h:t) =
   ndfawalk delta (epsilon_closure delta (delta' delta sts (Just h))) t

letter :: Char -> Bool
letter = (`elem` ['a'..'z']++['A'..'Z'])

digit :: Char -> Bool
digit = (`elem` ['0'..'9'])

ndfascii = Ndfa asciiTable [1,2,3] [1] [2] delta
            where   delta 3 _ = [3]
                    delta _ sy = case sy of 
                                Just x -> if x `elem` ['a'..'z']++['A'..'Z'] then [2]
                                          else [3]
                                Nothing -> [3]

isletter :: Char -> Bool
isletter = (ndfaaccept ndfascii) . (:[])

ndfaIdent = Ndfa asciiTable [1,2,3,4] [1] [2] delta
            where   delta 1 sy = case sy of 
                                Just x -> if (letter x) || x == '_' then [2]
                                          else [4]
                                Nothing -> [4]
                    delta 2 sy = case sy of 
                                Just x -> if letter x || digit x || x == '_' || x == '-' then [3]
                                          else [4]
                                Nothing -> [4]
                    delta 3 Nothing = [2]
                    delta _ _ = [4]


isIdent :: String -> Bool
isIdent = ndfaaccept ndfaIdent 


-- Robot
robot = Ndfa "edpl" [1,2,3,4,5] [1] [5] delta
        where delta 1 (Just 'd') = [2]
              delta 1 _ = [1]
              delta 2 (Just 'p') = [3]
              delta 2 (Just 'e') = [1]
              delta 2 _ = [2]
              delta 3 (Just 'e') = [4]
              delta 3 Nothing = [2]
              delta 3 _ = [3]
              delta 4 (Just 'l') = [5]
              delta 4 Nothing = [5]
              delta 4 (Just 'd') = [3]
              delta 4 _ = [4]
              delta 5 Nothing = [1]
              delta 5 _ = [5]

win :: String -> Bool
win = ndfaaccept robot

ndfadestinationsFrom :: Ord st => (st -> Maybe sy -> [st])       -- Tansition Function
                    -> [sy]                   -- Vocabulary
                    -> st                     -- Origin
                    -> [st]                   -- Destination States
ndfadestinationsFrom delta v o = nub $ concat [ndfawalk delta [o] [x] | x <- v]


ndfanumberIncomingArrows :: Ord st => (st -> Maybe sy -> [st])       -- Tansition Function
                        -> [sy]                   -- Vocabulary
                        -> [st]                   -- Set of States
                        -> st                     -- Destination
                        -> Int                   -- Origin States
ndfanumberIncomingArrows delta sys sts d = length $ (filter id) [d `elem` (ndfawalk delta (epsilon_closure delta sts) [v]) | v <- sys]











