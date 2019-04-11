module Transform where

import Data.List
import System.Process
import RegExp
import Dfa
import Ndfa
import RegExp2Aut

a1 :: DfaF Int Char
a1 = DfaF "ab" [1,2,3,4] 1 [3] delta
        where delta 1 'a' = 2
              delta 1 'b' = 1
              delta 2 'a' = 1
              delta 2 'b' = 3
              delta 3 'a' = 1
              delta 3 'b' = 2
              delta _ _   = 4

a2 :: DfaF Int Char
a2 = DfaF "abc" [1,2,3,4,5] 1 [4] delta
        where delta 1 'a' = 2
              delta 1 'c' = 4
              delta 2 'b' = 3
              delta 3 'c' = 4
              delta 4 'c' = 4
              delta _ _   = 5

nd1T = NdfaT "bdi;e" [1,2,3,4,5] [1] [5] delta
        where delta = [((1,Just 'b'),[2])
                      ,((2,Just 'd'),[3])
                      ,((2,Just 'i'),[3])
                      ,((2,Just 'e'),[5])
                      ,((3,Just ';'),[4])
                      ,((4,Nothing), [2])
                      ]

er1 = Or (Star (Literal 'a')) (Star (Literal 'b'))

regExp2dfa :: RegExp -> DfaT [Int] Char
regExp2dfa re = n2D $ regExp2Ndfa re

showProc e = do let fname = "__"
                putStrLn (showRE e)
                let an = regExp2Ndfa e
                let n  = fa2Dot an
                let ad = n2D an
                let d  = fa2Dot ad
                --let dnn = normStates ad
                --let dn = fa2Dot dnn
                writeFile (fname ++ "N.dot") (subsdoublequote n)
                writeFile (fname ++ "D.dot") (subsdoublequote d)
                --writeFile (fname ++ "DN.dot") (subsdoublequote dn)
                system ("dot -Tpng -O " ++ fname ++"N.dot")
                system ("dot -Tpng -O " ++ fname ++"D.dot")
                --system ("dot -Tpng -O " ++ fname ++"DN.dot")
                system ("open " ++ fname ++ "N.dot.png")
                system ("open " ++ fname ++ "D.dot.png")
                --system ("open " ++ fname ++ "DN.dot.png")


--getModification :: [((st,Maybe sy),[ st ])] -> [(([st],sy),[st])] -> Maybe (([st],sy),[st])
getModification _ [] = Nothing
getModification delta (((os,y),ds):xs) = if sort [eclosureSet delta [] (from delta s y) | s <- os] == sort ds 
                                            then getModification delta xs
                                         else Just ((os,y),ds)


-- Preciso do s2 porque o e-closure do NDFA == s2 e preciso do z2 porque todos os estados em q2 têm que ter um caminho até z2
-- putNdfa :: NdfaT st sy -> DfaT st sy -> NdfaT st sy
{- putNdfa (NdfaT v1 q1 s1 z1 delta1) (DfaT v2 q2 s2 z2 delta2) = case getModification delta1 delta2 of 
                                                                Just ((o,y),d) -> putNdfa (ndfaAddTransitions (NdfaT v1 q1 s1 z1 delta1) ((o,y),d)) (DfaT v2 q2 s2 z2 delta2)
                                                                Nothing -> (NdfaT v1 q1 s1 z1 delta1)
-}


-- 1º - validar que o DFA está bem construído
-- 2º Verificar as alterações em relação ao primeiro
-- 3º definir um put para trabalhar as alterações


