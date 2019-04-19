module Dfa where

--import Data.List
--import Data.Char

{- DFA where the transitions are defined as a function -}
data DfaF st sy = DfaF [sy]              -- Finite set of Vocabulary Symbols
                       [st]              -- Finite set of states
                       st                -- The start state
                       [st]              -- The set of final states
                       (st -> sy -> st)  -- Transition Function

{- DFA where the transitions are defined as a table -}
data Dfa st sy = Dfa [sy]              -- Finite set of Vocabulary Symbols
                     [st]              -- Finite set of states
                     st                -- The start state
                     [st]              -- The set of final states
                     [((st,sy),st)]    -- Transition Table


{- Converts all DFA into the tabulated version -}
class DFA t where
  injDFA :: t states voc -> Dfa states voc

instance DFA Dfa where
  injDFA = id

instance DFA DfaF where
  injDFA (DfaF voc states start finals delta) 
    = Dfa voc states start finals (tabulate delta states voc)

{- Given a delta function, the states and the vocabulary computes the transition table -}
tabulate :: (a -> b -> c) -> [a] -> [b] -> [((a,b),c)]
tabulate f as bs = [((x,y),f x y) | x <- as, y <- bs]


{- Given the trasitions table, an initial state and a string 'a' computes the pair (s,[a]) where:
    - s -> the state reached with the recognized symbols
    - a -> remaining of the list not recognized by the DFA
-}
tableWalk :: (Eq s, Eq a) => [((s,a),s)] -> s -> [a] -> (s,[a]) 
tableWalk delta s [] = (s,[])
tableWalk delta s (x:xs) = 
     case lookup (s,x) delta of
       Just s' -> tableWalk delta s' xs
       Nothing -> (s,(x:xs))

{- Given the transitions function, an initial state and a string 'a' computes the achieved state by 
  the recognition of 'a'
-}
functionwalk :: (st -> sy -> st) -> st -> [sy] -> st
functionwalk delta s [] = s
functionwalk delta s (x:xs) = functionwalk delta (delta s x) xs


{- Detemines if a given string belongs to the language defined by the DFA-}
dfaaccept :: (Eq st, Eq sy, DFA t) => t st sy -> [sy] -> Bool
dfaaccept a sentence =
       let (Dfa v q s z delta) = injDFA a 
           (f,remain) = tableWalk delta s sentence
       in (null remain) && (f `elem` z)

{- 
destinationsFrom :: (st -> sy -> st)       -- Transition Function
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
