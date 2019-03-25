module Dfa where

import Data.Char
import RegExp
import Data.Maybe
import Control.Monad

data Dfa st sy = Dfa [sy]              -- Finite set of Vocabulary Symbols
                     [st]              -- Finite set of states
                     st                -- The start state
                     [st]              -- The set of final states
                     (st -> sy -> st)  -- Transition Function

a1 :: Dfa Int Char
a1 = Dfa "ab" [1,2,3,4] 1 [3] delta
        where delta 1 'a' = 2
              delta 1 'b' = 1
              delta 2 'a' = 1
              delta 2 'b' = 3
              delta 3 'a' = 1
              delta 3 'b' = 2
              delta _ _   = 4

dfawalk :: (st -> sy -> st) -> st -> [sy] -> st
dfawalk delta s [] = s
dfawalk delta s (x:xs) = dfawalk delta (delta s x) xs

dfaaccept :: Eq st => Dfa st sy -> [sy] -> Bool
dfaaccept (Dfa v q s z delta) simb = (dfawalk delta s simb) `elem` z


a2 :: Dfa Int Char
a2 = Dfa "abc" [1,2,3,4,5] 1 [4] delta
        where delta 1 'a' = 2
              delta 1 'c' = 4
              delta 2 'b' = 3
              delta 3 'c' = 4
              delta 4 'c' = 4
              delta _ _   = 5

p2 = (a `Then` b `Then` (Star c)) `Or` c

iguais inp = matches p2 inp && dfaaccept a2 inp

asciiTable = map chr [0..255]

digits  :: Dfa Int Char
digits  = Dfa asciiTable [1,2,3] 1 [2] delta
  where   delta 1 x | x `elem` ['0'..'9'] = 2
          delta _ _                       = 3

-- Predicate to define whether a ascii symbol is a digit or not
isdigit    :: Char -> Bool
isdigit d  = dfaaccept digits [d]

lowerLetters  :: Dfa Int Char
lowerLetters  = Dfa asciiTable [1,2,3] 1 [2] delta
  where  delta 1 x | x `elem` ['a'..'z'] = 2
         delta _ _                       = 3

-- Predicate to define whether a ascii symbol is a lower letter or not

islower    :: Char -> Bool
islower l  = dfaaccept lowerLetters [l]


destinationsFrom :: (st -> sy -> st)       -- Tansition Function
                 -> [sy]                   -- Vocabulary
                 -> st                     -- Origin
                 -> [st]                   -- Destination States
destinationsFrom delta vs o = [ delta o v | v <- vs ]



numberIncomingArrows :: Eq st
                     => (st -> sy -> st)       -- Transition Function
                     -> [sy]                   -- Vocabulary
                     -> [st]                   -- Set of States
                     -> st                     -- Destination
                     -> Int                    -- Number of Arrows
numberIncomingArrows d vs qs dest = length  [ q
                                            | v <- vs
                                            , q <- qs 
                                            , d q v == dest
                                            ]

dfaaccept' :: Eq st => Dfa st sy -> [sy] -> Bool
dfaaccept' (Dfa v q s z delta) simb = foldl delta s simb `elem` z


realExpReg =  (Optional ((Literal '-') `Or` (Literal '+')))
              `Then` (Star digitos) 
              `Then` (Optional (Literal '.')) 
              `Then` (OneOrMore digitos)

realDfa = Dfa ['+','-','.','0','1','2','3','4','5','6','7','8','9']
              ['A','B','C','D','E'] 
              'A'
              ['C','E']
              deltaReal

deltaReal 'A' '+' = 'B'
deltaReal 'A' '-' = 'B' 
deltaReal 'A' '0' = 'C'
deltaReal 'A' '1' = 'C'
deltaReal 'A' '2' = 'C'
deltaReal 'A' '3' = 'C'
deltaReal 'A' '4' = 'C'
deltaReal 'A' '5' = 'C'
deltaReal 'A' '6' = 'C'
deltaReal 'A' '7' = 'C'
deltaReal 'A' '8' = 'C'
deltaReal 'A' '9' = 'C'

deltaReal 'B' '0' = 'C'
deltaReal 'B' '1' = 'C'
deltaReal 'B' '2' = 'C'
deltaReal 'B' '3' = 'C'
deltaReal 'B' '4' = 'C'
deltaReal 'B' '5' = 'C'
deltaReal 'B' '6' = 'C'
deltaReal 'B' '7' = 'C'
deltaReal 'B' '8' = 'C'
deltaReal 'B' '9' = 'C'

deltaReal 'C' '0' = 'C'
deltaReal 'C' '1' = 'C'
deltaReal 'C' '2' = 'C'
deltaReal 'C' '3' = 'C'
deltaReal 'C' '4' = 'C'
deltaReal 'C' '5' = 'C'
deltaReal 'C' '6' = 'C'
deltaReal 'C' '7' = 'C'
deltaReal 'C' '8' = 'C'
deltaReal 'C' '9' = 'C'
deltaReal 'C' '.' = 'D'
deltaReal 'D' '0' = 'E'
deltaReal 'D' '1' = 'E'
deltaReal 'D' '2' = 'E'
deltaReal 'D' '3' = 'E'
deltaReal 'D' '4' = 'E'
deltaReal 'D' '5' = 'E'
deltaReal 'D' '6' = 'E'
deltaReal 'D' '7' = 'E'
deltaReal 'D' '8' = 'E'
deltaReal 'D' '9' = 'E'

deltaReal 'E' '0' = 'E'
deltaReal 'E' '1' = 'E'
deltaReal 'E' '2' = 'E'
deltaReal 'E' '3' = 'E'
deltaReal 'E' '4' = 'E'
deltaReal 'E' '5' = 'E'
deltaReal 'E' '6' = 'E'
deltaReal 'E' '7' = 'E'
deltaReal 'E' '8' = 'E'
deltaReal 'E' '9' = 'E'





