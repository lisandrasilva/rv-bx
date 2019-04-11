module Dfa where

import Data.List
import Data.Char
import RegExp

data DfaF st sy = DfaF [sy]              -- Finite set of Vocabulary Symbols
                       [st]              -- Finite set of states
                       st                -- The start state
                       [st]              -- The set of final states
                       (st -> sy -> st)  -- Transition Function

data DfaT st sy = DfaT [sy]              -- Finite set of Vocabulary Symbols
                       [st]              -- Finite set of states
                       st                -- The start state
                       [st]              -- The set of final states
                       [((st,sy),st)]    -- Transition Table

class DFA t where
  injDFA :: t states voc -> DfaT states voc

instance DFA DfaT where
  injDFA = id

instance DFA DfaF where
  injDFA (DfaF voc states start finals delta) 
    = DfaT voc states start finals (tabulate delta states voc)

tabulate :: (a -> b -> c) -> [a] -> [b] -> [((a,b),c)]
tabulate f as bs = [((x,y),f x y) | x <- as, y <- bs]

tableWalk :: (Eq s, Eq a) => [((s,a),s)] -> s -> [a] -> (s,[a]) 
tableWalk delta s [] = (s,[])
tableWalk delta s (x:xs) = 
     case lookup (s,x) delta of
       Just s' -> tableWalk delta s' xs
       Nothing -> (s,(x:xs))

dfawalk :: (st -> sy -> st) -> st -> [sy] -> st
dfawalk delta s [] = s
dfawalk delta s (x:xs) = dfawalk delta (delta s x) xs



dfaaccept :: (Eq st, Eq sy, DFA t) => t st sy -> [sy] -> Bool
dfaaccept a sentence =
       let (DfaT v q s z delta) = injDFA a 
           (f,remain) = tableWalk delta s sentence
       in (null remain) && (f `elem` z)


dfaAddTransition :: (Eq st, Eq sy) => DfaT st sy -> ((st,sy),st) -> DfaT st sy
dfaAddTransition (DfaT v q s z delta) ((d,y),o) = case lookup (d,y) delta of
                                            Just s' -> (DfaT v q s z delta)
                                            Nothing -> (DfaT (v `union` [y]) (q `union` [d,o]) s z (((d,y),o):delta))

{- }
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

-}

-- Examples 


a1 :: DfaT Int Char
a1 = DfaT "ab" [1,2,3] 1 [3] delta
        where delta = [((1,'a'),2)
                      ,((1,'b'),1)
                      ,((2,'a'),1)
                      ,((2,'b'),3)
                      ,((3,'a'),1)
                      ,((3,'b'),2)
                      ]

a2 :: DfaT Int Char
a2 = DfaT "abc" [1,2,3,4] 1 [4] delta
        where delta = [((1,'a'),2)
                      ,((1,'c'),4)
                      ,((2,'b'),3)
                      ,((3,'c'),4)
                      ,((4,'c'),4)
                      ]

p2 = (a `Then` b `Then` (Star c)) `Or` c

iguais inp = matches p2 inp && dfaaccept a2 inp

asciiTable = map chr [0..255]

digits  :: DfaF Int Char
digits  = DfaF asciiTable [1,2,3] 1 [2] delta
  where   delta 1 x | x `elem` ['0'..'9'] = 2
          delta _ _                       = 3

-- Predicate to define whether a ascii symbol is a digit or not
isdigit    :: Char -> Bool
isdigit d  = dfaaccept digits [d]

lowerLetters  :: DfaF Int Char
lowerLetters  = DfaF asciiTable [1,2,3] 1 [2] delta
  where  delta 1 x | x `elem` ['a'..'z'] = 2
         delta _ _                       = 3

-- Predicate to define whether a ascii symbol is a lower letter or not

islower    :: Char -> Bool
islower l  = dfaaccept lowerLetters [l]

realExpReg =  (Optional ((Literal '-') `Or` (Literal '+')))
              `Then` (Star digitos) 
              `Then` (Optional (Literal '.')) 
              `Then` (OneOrMore digitos)

realDfa = DfaF ['+','-','.','0','1','2','3','4','5','6','7','8','9']
              ['A','B','C','D','E'] 
              'A'
              ['C','E']
              deltaReal
   where deltaReal 'A' '+' = 'B'
         deltaReal 'A' '-' = 'B' 
         deltaReal 'A' d | d>='0' && d<='9' = 'C'
         deltaReal 'B' d | d>='0' && d<='9' = 'C'
         deltaReal 'C' d | d>='0' && d<='9' = 'C'
         deltaReal 'C' '.' = 'D'
         deltaReal 'D' d | d>='0' && d<='9' = 'E'
         deltaReal 'E' d | d>='0' && d<='9' = 'E'

dfaInt = DfaT ('-':'+':['0'..'9']) [1,2,3,4] 1 [4] delta
            where   delta = [((1,'+'),2),((1,'-'),3)]
                        ++ [((s,x),4) | s<-[2,3], x<- ['0'..'9'] ]




