--import Control.Exception
import Data.List
import System.Process
import RegExp
import Dfa
import Ndfa
import Ndfa2Dfa

data NdfaT st sy = NdfaT  [ sy ]  -- Vocabulary
                          [ st ]  -- Finite set of states
                          [ st ]  -- The set of start states
                          [ st ]  -- The set of final states
                          [((st,Maybe sy),[st])]  -- Tabulated transition function

data DfaT st sy = DfaT [sy]           -- Finite set of Vocabulary Symbols
                       [st]           -- Finite set of states
                       st             -- The start state
                       [st]           -- The set of final states
                       [((st,sy),st)] -- Tabulated Transition Function


instance (Show st, Show sy) => Show (Dfa st sy) where
     show = fa2Str

instance (Show st, Show sy) => Show (DfaT st sy) where
     show = fa2Str

instance (Show st, Show sy) => Show (Ndfa st sy) where
     show = fa2Str

instance (Show st, Show sy) => Show (NdfaT st sy) where
     show = fa2Str

a1 :: Dfa Int Char
a1 = Dfa "ab" [1,2,3,4] 1 [3] delta
        where delta 1 'a' = 2
              delta 1 'b' = 1
              delta 2 'a' = 1
              delta 2 'b' = 3
              delta 3 'a' = 1
              delta 3 'b' = 2
              delta _ _   = 4

a2 :: Dfa Int Char
a2 = Dfa "abc" [1,2,3,4,5] 1 [4] delta
        where delta 1 'a' = 2
              delta 1 'c' = 4
              delta 2 'b' = 3
              delta 3 'c' = 4
              delta 4 'c' = 4
              delta _ _   = 5

nd1 = Ndfa "bdi;e" [1,2,3,4,5,6] [1] [5] delta
        where delta 1 (Just 'b') = [2]
              delta 2 (Just 'd') = [3]
              delta 2 (Just 'i') = [3]
              delta 2 (Just 'e') = [5]
              delta 3 (Just ';') = [4]
              delta 4 Nothing = [2]
              delta _ _ = [6]

nd1T = NdfaT "bdi;e" [1,2,3,4,5] [1] [5] delta
        where delta = [((1,Just 'b'),[2])
                      ,((2,Just 'd'),[3])
                      ,((2,Just 'i'),[3])
                      ,((2,Just 'e'),[5])
                      ,((3,Just ';'),[4])
                      ,((4,Nothing), [2])
                      ]

ex = Ndfa "ab" "ABCDE" "A" "D" delta
        where delta 'A' (Just 'a') = "BC"
              delta 'A' Nothing = "D"
              delta 'B' (Just 'a') = "C"
              delta 'C' (Just 'b') = "D"
              delta 'D' Nothing = "C"
              delta _ _ = "E"
---------------

class FiniteAut a where
  injNdfaT :: a st sy -> NdfaT st sy

instance FiniteAut NdfaT where
  injNdfaT = id 

instance FiniteAut Ndfa where
  -- injNdfaT :: Ndfa st sy -> NdfaT st sy
     injNdfaT (Ndfa voc st start final delta)
      = NdfaT voc st start final (tab delta st (Nothing:map Just voc))
        where tab f x y = [((i,a),f i a) | i <- x, a <- y]

instance FiniteAut Dfa where
  -- injNdfaT :: Dfa st sy -> NdfaT st sy
     injNdfaT (Dfa voc st start final delta)
      = NdfaT voc st [start] final (tab delta st voc)
        where tab f x y = [((i,Just a),[f i a]) | i <- x, a <- y]

instance FiniteAut DfaT where
  -- injNdfaT :: DfaT st sy -> NdfaT st sy
     injNdfaT (DfaT voc st start final delta)
      = NdfaT voc st [start] final (map (\ ((x,y),z) -> ((x,(Just y)),[z]) ) delta)

---------------

fa2Str :: (FiniteAut t, Show st, Show sy) => t st sy -> String
fa2Str a = 
     let (NdfaT voc stats starts final delta) = injNdfaT a
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

fa2Dot :: (FiniteAut t, Show st, Show sy, Eq st) => t st sy -> String
fa2Dot a = header ++ states ++ initials ++ transitions ++ footer
    where (NdfaT voc stats start final delta) = injNdfaT a
          header = unlines ["digraph G {"
                           , "    rankdir=LR;"
                           ,"    fontname=\"sans-serif\";"
                           , "    penwidth=\"0.1\";"
                           , "    edge [comment=\"Wildcard edge\", "
                           , "          fontname=\"sans-serif\", "
                           , "          fontsize=10, "
                           , "          colorscheme=\"blues3\"," 
                           , "          color=2, "
                           , "          fontcolor=3];"
                           , "    node [fontname=\"serif\", "
                           , "          fontsize=13, "
                           , "          fillcolor=\"1\", "
                           , "          colorscheme=\"blues4\", "
                           , "          color=\"2\", "
                           , "          fontcolor=\"4\"," 
                           , "          style=\"filled\"];"
                           , "    \"Init\" [fontsize=\"0\"," 
                           , "            fillcolor=\"white\"," 
                           , "            penwidth=\"0.0\"];"
                           ]
          footer = unlines ["}"
                           ]
          finalstates = unlines $ map ((\x-> "    " ++ x ++ " [shape=\"doublecircle\"];").showWithQuote) final
          nonfinalstates = unlines $ map ((\x-> "    " ++ x ++ ";").showWithQuote) ((init stats) \\ final)
          states = nonfinalstates ++ finalstates
          transitions = unlines $ map transition $ filter (\((_,_),c') -> (c' /= last stats)) [((a,b),c') | ((a,b),c) <- delta , c' <- c]
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

showWithQuoteM :: Show a => Maybe a -> String
showWithQuoteM (Just x) = "\"" ++ show x ++ "\""
showWithQuoteM Nothing  = "\"@\""

displayFA d = do writeFile "__.dot" (fa2Dot d)
                 system "dot -Tpng -O __.dot"
                 system "open __.dot.png"

------------------

revNTab :: (Eq st, Eq sy) => [((st,Maybe sy),[st])] -> [((st,Maybe sy),[st])]
revNTab tab = agrupa [((z',y),x) | ((x,y),z) <- tab, z' <- z]
    where agrupa [] = []
          agrupa (((a,b),c):t) = let (x,t') = remove (a,b) t
                                in ((a,b),c:x):agrupa t
          remove _ [] = ([],[])
          remove s' ((s,d):t) | s == s' = ((d:x'), t')
                              | otherwise = (x', (s,d):t')
                      where (x',t') = remove s' t

revAut :: (FiniteAut t, Eq st, Eq sy)=> t st sy -> NdfaT st sy
revAut a = 
     let (NdfaT voc stats starts final delta) = injNdfaT a
     in NdfaT voc stats final starts (revNTab delta)
