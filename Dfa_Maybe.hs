module Dfa where

import Data.Char
import RegExp
import Data.Maybe
import Control.Monad

data Dfa st sy = Dfa [sy]              -- Finite set of Vocabulary Symbols
                     [st]              -- Finite set of states
                     st                -- The start state
                     [st]              -- The set of final states
                     (st -> sy -> Maybe st)  -- Transition Function

a1 :: Dfa Int Char
a1 = Dfa "ab" [1,2,3] 1 [3] delta
        where delta 1 'a' =  Just 2
              delta 1 'b' =  Just 1
              delta 2 'a' =  Just 1
              delta 2 'b' =  Just 3
              delta 3 'a' =  Just 1
              delta 3 'b' =  Just 2
              delta _ _   = Nothing

dfawalk :: (st -> sy -> Maybe st) -> st -> [sy] -> Maybe st
dfawalk delta s [] = Just s
dfawalk delta s (x:xs) = case delta s x of   
                            Just ns -> dfawalk delta ns xs
                            Nothing -> Nothing

dfaaccept :: Eq st => Dfa st sy -> [sy] -> Bool
dfaaccept (Dfa v q s z delta) simb = case (dfawalk delta s simb) of 
                                        Just ns -> ns `elem` z
                                        Nothing -> False


a2 :: Dfa Int Char
a2 = Dfa "abc" [1,2,3,4] 1 [4] delta
        where delta 1 'a' = Just 2
              delta 1 'c' = Just 4
              delta 2 'b' = Just 3
              delta 3 'c' = Just 4
              delta 4 'c' = Just 4
              delta _ _   = Nothing

p2 = (a `Then` b `Then` (Star c)) `Or` c

iguais inp = matches p2 inp && dfaaccept a2 inp

asciiTable = map chr [0..255]

digits  :: Dfa Int Char
digits  = Dfa asciiTable [1,2,3] 1 [2] delta
  where   delta 1 x | x `elem` ['0'..'9'] = Just 2
          delta _ _                       = Nothing

-- Predicate to define whether a ascii symbol is a digit or not

isdigit    :: Char -> Bool
isdigit d  = dfaaccept digits [d]

lowerLetters  :: Dfa Int Char
lowerLetters  = Dfa asciiTable [1,2,3] 1 [2] delta
  where  delta 1 x | x `elem` ['a'..'z'] = Just 2
         delta _ _                       = Nothing

-- Predicate to define whether a ascii symbol is a lower letter or not

islower    :: Char -> Bool
islower l  = dfaaccept lowerLetters [l]


destinationsFrom :: (st -> sy -> Maybe st)       -- Tansition Function
                 -> [sy]                   -- Vocabulary
                 -> st                     -- Origin
                 -> [st]                   -- Destination States
destinationsFrom delta vs o = catMaybes [ delta o v | v <- vs ]



numberIncomingArrows :: Eq st
                     => (st -> sy -> Maybe st)       -- Transition Function
                     -> [sy]                   -- Vocabulary
                     -> [st]                   -- Set of States
                     -> st                     -- Destination
                     -> Int                    -- Number of Arrows
numberIncomingArrows d vs qs dest = length  [ q
                                            | v <- vs
                                            , q <- qs 
                                            , d q v == Just dest
                                            ]

dfaaccept' :: Eq st => Dfa st sy -> [sy] -> Bool
dfaaccept' (Dfa v q s z delta) simb = case (foldM delta s simb) of 
                                        Just x -> x `elem` z 
                                        Nothing -> False





