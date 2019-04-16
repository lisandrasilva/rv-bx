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

er1 = Or (Then (Literal 'b') (Star (Literal 'a'))) (Then (Literal 'a') (Star (Literal 'b')))

ex = NdfaT "ab" "ABCDE" "A" "D" delta
        where delta = [(('A',Just 'a'),"BC")
                      ,(('A',Nothing),"D")
                      ,(('B',Just 'a'),"C")
                      ,(('C',Just 'b'),"D")
                      ,(('C',Just 'a'),"E")
                      ,(('E',Just 'b'),"D")
                      ,(('D',Nothing),"E")]

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



-- Dado o delta do ndfa e o delta do dfa percorre o delta do dfa e verifica se o walk pelo ndfa bate certo
getModification :: (Ord a, Eq t1) => [((a, Maybe t1), [a])] -> [(([a], t1), [a])] -> Maybe (([a], t1), [a])
getModification _ [] = Nothing
getModification delta (((os,y),ds):xs) = if (sort $ eclosureSet delta [] (from delta os y)) == sort ds 
                                            then getModification delta xs
                                         else Just ((os,y),ds)

-- Função para verificar que um DFA está topologicamente bem construído
-- para cada ((o,s),d) verificar que:
--       o par (o,s) só aparece uma vez
--       'o' é 'd' em algum outro par excepto o primeiro
--       'd' é 'o' em algum outro par
--       existe sempre um caminho entre um nodo e um nodo final
--       existe sempre um caminho entre o nodo inicial e qualquer nodo
wellBuilt :: (Eq t, Eq a) => DfaT a t -> Bool
wellBuilt (DfaT v q s z delta) = unique delta && 
                                 (os \\ ds == [s]) && 
                                 (null $ ds \\ os) &&
                                 q \\ (reachableNodes delta [s] []) == [] &&
                                 q \\ (reachableNodes atled z []) == []
    where os = nub $ map (fst . fst) delta
          ds = nub $ map snd delta
          atled = [((d,y),o) | ((o,y),d) <- delta]
          unique [] = True 
          unique [x] = True
          unique (((o,y),d):xs) = case lookup (o,y) xs of
                                    Just x -> False
                                    Nothing -> unique xs


reachableNodes :: Eq a => [((a, t), a)] -> [a] -> [a] -> [a]
reachableNodes _ [] acc = acc
reachableNodes delta (n:ns) acc = reachableNodes delta x (n:acc)
    where x = ns `union` [ d | ((o,y),d) <- delta , o == n , not (d `elem` (n:acc))]


-- Função para verificar que um automato está consistente ????
-- Função para mostrar que 2 grafos são isomorfos



-- ver os que são novos estados e os que são estados finais
-- ir buscar todas as transições que já existem de 'o' por y. e acrescentar lá os ds
ndfaAddTransitions (NdfaT v1 q1 s1 z1 delta) ((os,y),ds) = let newtransitions = [((o,Just y),ds) | o <- os]
                                                               newstates = [s | s <- (os `union` ds), not (s `elem` q1)]
                                                            in (NdfaT v1 (q1 ++ newstates) s1 z1 (delta ++ newtransitions))
           

dfaAddTransitions :: (Eq st, Eq sy) => DfaT st sy -> [((st,sy),st)] -> DfaT st sy
dfaAddTransitions dfa [] = dfa
dfaAddTransitions dfa@(DfaT v q s z delta) (((d,y),o):xs) = let (DfaT v1 q1 s1 z1 delta1) = dfaAddTransitions dfa xs
                                                        in (DfaT v1 q1 s1 z1 (delta ++ delta1))

-- Alterar para receber uma flag para o caso de se querer que o estado seja final
dfaAddTransition :: (Eq st, Eq sy) => DfaT st sy -> ((st,sy),st) -> DfaT st sy
dfaAddTransition dfa@(DfaT v q s z delta) ((d,y),o) = 
    case lookup (d,y) delta of
      Just s' -> dfa
      Nothing -> (DfaT (nub(v `union` [y])) (q `union` [d,o]) s z (delta ++ [((d,y),o)]))

-- Preciso do s2 porque o e-closure do NDFA == s2 e preciso do z2 porque todos os estados em q2 têm que ter um caminho até z2
-- Pôr isto a retornar erro caso o automato não esteja bem contruído
putNdfa' :: (Ord st, Eq sy) => NdfaT st sy -> DfaT [st] sy -> NdfaT st sy
putNdfa' ndfa@(NdfaT v1 q1 s1 z1 delta1) dfa@(DfaT v2 q2 s2 z2 delta2) = case getModification delta1 delta2 of 
                                                                      Just ((o,y),d) -> putNdfa' (ndfaAddTransitions ndfa ((o,y),d)) dfa
                                                                      Nothing -> (NdfaT v1 q1 s1 z1 delta1)


putNdfa :: (Ord st, Eq sy) => NdfaT st sy -> DfaT [st] sy -> Either String (NdfaT st sy)
putNdfa ndfa@(NdfaT v1 q1 s1 z1 delta1) dfa@(DfaT v2 q2 s2 z2 delta2) = if wellBuilt dfa then Right (putNdfa' ndfa dfa)
                                                                        else Left "View is not correct"


-- 1º - validar que o DFA está bem construído
-- 2º Verificar as alterações em relação ao primeiro
-- 3º definir um put para trabalhar as alterações


