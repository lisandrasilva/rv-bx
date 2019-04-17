module ShowAut where

import Data.List
import System.Process
import Ndfa
import Dfa
import Ndfa2Dfa
import RegExp
import RegExp2Aut
import Examples

instance (Show st, Show sy, Eq st) => Show (DfaF st sy) where
     show = showAut

instance (Show st, Show sy, Eq st) => Show (DfaT st sy) where
     show = showAut

instance (Show st, Show sy, Eq st) => Show (NdfaF st sy) where
     show = showAut

instance (Show st, Show sy, Eq st) => Show (NdfaT st sy) where
     show = showAut


{- Definition of show for automata -}
showAut :: (NDFA t, Show st, Show sy, Eq st) => t st sy -> String
showAut a = 
     let (NdfaT voc stats starts final delta) = injNDFA a
         indent pat l = zipWith (++) ("":(repeat pat)) l
         showLComma l = concat (intersperse ", " (map show l))
         showTabI = concat 
                  . (intersperse "\n") 
                  . (indent "                ")
                  . (map (\((x,y),z) -> "(" ++ x ++ "," ++ y ++ ") -> " ++ z) )
     in      "Vocabulary:     " ++ showLComma voc 
        ++ "\nStates:         " ++ showLComma stats 
        ++ "\nStart state(s): " ++ showLComma starts
        ++ "\nFinal states:   " ++ showLComma final
        ++ "\nTransitions:    " ++ showTabI [((show x,show y),show z') | ((x,y),z) <- delta, z' <- z]


{- Converts an automaton in a string to write in the .dot file -}
fa2Dot :: (NDFA t, Show st, Show sy, Eq st) => t st sy -> String
fa2Dot a = header ++ states ++ initials ++ transitions ++ footer
    where (NdfaT voc stats start final delta) = injNDFA a
          header = unlines [ "digraph G {"
                           , "    rankdir=LR;"
                           , "    fontname=\"sans-serif\";"
                           , "    penwidth=\"0.1\";"
                           , "    edge [comment=\"Wildcard edge\", "
                           , "          shape=\"circle\", "
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
                          ++ showWithQuote l
                          ++ "];"
          initials = unlines $ map initSt start
          initSt x = "    \"Init\" -> "
                     ++ showWithQuote x
                     ++ ";"

showWithQuote :: Show a => a -> String
showWithQuote x = "\"" ++ show x ++ "\""

{- Removes quotes from a String -}
subsdoublequote :: String -> String
subsdoublequote = subs True 
  where subs True  ('\"':'\"':t) = '\"':'\\':'\"':subs False t
        subs False ('\"':'\"':t) = '\\':'\"':'\"':subs True t
        subs flag (h:t) = h:(subs flag t)
        subs _ [] = []

er1 = Or (Then (Literal 'a') (Star (Literal 'a'))) (Then (Literal 'a') (Star (Literal 'b')))

displayFA d = do writeFile "__.dot" (subsdoublequote (fa2Dot d))
                 system "dot -Tpng -O __.dot"
                 system "open __.dot.png"

