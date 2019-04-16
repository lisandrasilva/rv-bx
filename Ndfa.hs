module Ndfa where

-- Defines Non deterministic finite automata
-- Convert these automata into (tabulated) deterministic finite automata
-- show instances of automata
-- display (.dot) for automata

import System.Process
import Data.List
import Dfa                     

-- Functional version 

data NdfaF st sy = NdfaF [ sy ]                      -- Vocabulary
                         [ st ]                      -- States
                         [ st ]                      -- Start states
                         [ st ]                      -- Final states
                         (st -> Maybe sy -> [ st ] ) -- Transition function

delta :: NdfaF st sy -> (st -> Maybe sy -> [ st ])
delta (NdfaF _ _ _ _ x) = x

-- Tabulated version

data NdfaT st sy = NdfaT [ sy ]                   -- Vocabulary
                         [ st ]                   -- States
                         [ st ]                   -- Start states
                         [ st ]                   -- Final states
                         [((st,Maybe sy),[ st ])] -- Transition table

-- Abstracting the form of how the transitions are stored

class NDFA t where
  injNDFA :: t states voc -> NdfaT states voc

instance NDFA NdfaT where
  injNDFA = id

instance NDFA NdfaF where
  injNDFA (NdfaF voc states starts finals delta) 
    = NdfaT voc states starts finals (tabulateM delta states voc)

tabulateM :: (a -> (Maybe b) -> [c]) -> [a] -> [b] -> [((a,Maybe b),[c])]
tabulateM f as bs = [((x,Just y),f x (Just y)) | x <- as, y <- bs] ++
                    [((x,Nothing),f x Nothing) | x <- as]

-- Deterministic automata can be made into non-deterministic 

toNonDet :: DFA t => t states voc -> NdfaT states voc
toNonDet a = let (DfaT v s i f tabel) = injDFA a
             in NdfaT v s [i] f [((x,Just y),[z]) | ((x,y),z) <- tabel]

instance NDFA DfaF where 
   injNDFA = toNonDet . injDFA 

instance NDFA DfaT where 
   injNDFA = toNonDet . injDFA 

-- Show function for automata
-- First for the tabulated non deterministic case

fa2Str :: (NDFA t, Show st, Show sy, Eq st) => t st sy -> String
fa2Str a = 
     let (NdfaT voc stats starts final delta) = injNDFA a
         indent pat l = zipWith (++) ("":(repeat pat)) l
         showLComma l = concat (intersperse ", " (map show l))
         showM (Just x) = show x
         showM Nothing = "@"
         showTabI = concat 
                  . (intersperse "\n") 
                  . (indent "                ")
                  . (map (\((x,y),z) -> "(" ++ x ++ "," ++ y ++ ") -> " ++ z) )
     in      "Vocabulary:     " ++ showLComma voc 
        ++ "\nStates:         " ++ showLComma stats 
        ++ "\nStart state(s): " ++ showLComma starts
        ++ "\nFinal states:   " ++ showLComma final
        ++ "\nTransitions:    " ++ showTabI [((show x,showM y),show z') | ((x,y),z) <- delta, z' <- z]

-- and now instatiating it for that 4 cases

instance (Show st, Show sy, Eq st) => Show (DfaF st sy) where
  show = fa2Str
instance (Show st, Show sy, Eq st) => Show (DfaT st sy) where
  show = fa2Str
instance (Show st, Show sy, Eq st) => Show (NdfaF st sy) where
  show = fa2Str
instance (Show st, Show sy, Eq st) => Show (NdfaT st sy) where
  show = fa2Str

-- graphviz displaying of automata

fa2Dot :: (NDFA t, Show st, Show sy, Eq st) => t st sy -> String
fa2Dot a = header ++ states ++ initials ++ transitions ++ footer
    where (NdfaT voc stats start final delta) = injNDFA a
          header = unlines [ "digraph G {"
                           , "    rankdir=LR;"
                           , "    fontname=\"sans-serif\";"
                           , "    penwidth=\"0.1\";"
                           , "    edge [comment=\"Wildcard edge\", "
                           , "          fontname=\"sans-serif\", "
                           , "          fontsize=10, "
                           , "          colorscheme=\"blues3\"," 
                           , "          color=2, "
                           , "          fontcolor=3];"
                           , "    node [fontname=\"serif\", "
                           , "          fontsize=13, "
                           , "          shape=\"circle\", "
                           , "          fillcolor=\"1\", "
                           , "          colorscheme=\"blues4\", "
                           , "          color=\"2\", "
                           , "          fontcolor=\"4\"," 
                           , "          style=\"filled\"];"
                           , "    \"Init\" [fontsize=\"0\"," 
                           , "            fillcolor=\"white\"," 
                           , "            penwidth=\"0.0\"];"
                           ]
          footer = unlines [ "}"
                           ]
          finalstates = unlines $ map ((\x-> "    " ++ x ++ " [shape=\"doublecircle\"];").showWithQuote) final
          nonfinalstates = unlines $ map ((\x-> "    " ++ x ++ ";").showWithQuote) (stats \\ final)
          states = nonfinalstates ++ finalstates
          transitions = unlines $ map transition [((a,b),c') | ((a,b),c) <- delta , c' <- c]
          transition ((i,l),f) = "    " 
                          ++ showWithQuote i 
                          ++ " -> "
                          ++ showWithQuote f
                          ++ "  [label=" 
                          ++ showWithQuoteM l
                          ++ "];"
          initials = unlines $ map initSt start
          initSt x = "    \"Init\" -> "
                     ++ showWithQuote x
                     ++ ";"

showWithQuote :: Show a => a -> String
showWithQuote x = "\"" ++ show x ++ "\""

subsdoublequote :: String -> String
subsdoublequote = subs True 
  where subs True  ('\"':'\"':t) = '\"':'\\':'\"':subs False t
        subs False ('\"':'\"':t) = '\\':'\"':'\"':subs True t
        subs flag (h:t) = h:(subs flag t)
        subs _ [] = []     

showWithQuoteM :: Show a => Maybe a -> String
showWithQuoteM (Just x) = "\"" ++ show x ++ "\""
showWithQuoteM Nothing  = "\"@\""

displayFA d = do writeFile "__.dot" (subsdoublequote (fa2Dot d))
                 system "dot -Tpng -O __.dot"
                 system "open __.dot.png"

-- Reversing an automaton

revNTab :: (Eq st, Eq sy) => [((st,Maybe sy),[st])] -> [((st,Maybe sy),[st])]
revNTab tab = agrupa [((z',y),x) | ((x,y),z) <- tab, z' <- z]
    where agrupa [] = []
          agrupa (((a,b),c):t) = let (x,t') = remove (a,b) t
                                in ((a,b),c:x):agrupa t
          remove _ [] = ([],[])
          remove s' ((s,d):t) | s == s' = ((d:x'), t')
                              | otherwise = (x', (s,d):t')
                      where (x',t') = remove s' t

revAut :: (NDFA t, Eq st, Eq sy)=> t st sy -> NdfaT st sy
revAut a = 
     let (NdfaT voc stats starts final delta) = injNDFA a
     in NdfaT voc stats final starts (revNTab delta)

-- Renaming the states 

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

-- Convert a non-deterministic automaton into a deterministic one
-- Rabin–Scott powerset construction

n2D :: (NDFA t, Ord st, Eq sy) => t st sy -> DfaT [st] sy
n2D a = let (NdfaT voc stats starts finals delta) = injNDFA a
            starts' = eclosureSet delta [] starts
            finals' = [x | x<-stats', not(null(x `intersect` finals))]
            (stats',delta') = buildTable delta [] [] [starts']
        in DfaT voc stats' starts' finals' (nub delta')




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


-- ver os que são novos estados e os que são estados finais
-- ir buscar todas as transições que já existem de 'o' por y. e acrescentar lá os ds
ndfaAddTransitions (NdfaT v1 q1 s1 z1 delta) ((os,y),ds) = let newtransitions = [((o,Just y),ds) | o <- os]
                                                               newstates = [s | s <- (os `union` ds), not (s `elem` q1)]
                                                            in (NdfaT v1 (q1 ++ newstates) s1 z1 (delta ++ newtransitions))
           

--isJust :: Maybe a -> Bool
--isJust (Just x) = True
--isJust Nothing = False
-- 
-- 
{- }

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

-}
-- Examples


-- b \ ((d + i);)^* \  e
nd1 = NdfaT "bdi;e" [1,2,3,4,5] [1] [5] delta
        where delta= delta1
delta1 = [((1,Just 'b'),[2])
                    ,((2,Just 'd'),[3])
                    ,((2,Just 'i'),[3])
                    ,((2,Just 'e'),[5])
                    ,((3,Just ';'),[4])
                    ,((4,Nothing  ),[2])
                    --,((2,Nothing  ),[3])
                    --,((3,Nothing  ),[1])
                    ]

{-
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

-}








