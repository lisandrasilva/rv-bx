module RegExp2Aut where

import Data.List
import RegExp
import Dfa
import Ndfa


regExp2Ndfa :: RegExp -> NdfaT Int Char
regExp2Ndfa er = fst (regExp2Ndfa' er 1)

regExp2Ndfa' :: RegExp -> Int -> (NdfaT Int Char,Int)
-- regExp2Ndfa e n = (a,p) where a is an automaton with states n..(p-1)
regExp2Ndfa' Empty n = ( NdfaT [] [sa,za] [sa] [za] delta , n+2 )
    where sa = n
          za = n+1
          delta = []

regExp2Ndfa' (Epsilon) n = ( NdfaT [] [sa] [sa] [sa] delta , n+1 )
    where sa = n 
          delta = []

regExp2Ndfa' (Literal l) n = ( NdfaT [l] [sa,za] [sa] [za] delta , n+2)
  where sa = n
        za = n+1
        delta=[((n,Just l),[n+1])]


regExp2Ndfa' (Then p q) n = ( NdfaT v' q' s' z' delta' , nq)
  where (NdfaT vp qp sp zp dp , np) = regExp2Ndfa' p n
        (NdfaT vq qq sq zq dq , nq) = regExp2Ndfa' q np
        v' = vp ++ vq
        q' = qp ++ qq
        s' = sp
        z' = zq
        delta' = (joinFinals dp zp sq) ++ dq
        -- há aqui um erro. 
        --Tem que se ver se os estados finais do primeiro não vão para outros sitios. 
        --nesse caso será preciso colapsar isto
regExp2Ndfa' (Or p q) n = ( NdfaT v' q' s' z' delta' , nq )
  where (NdfaT vp qp sp zp dp , np) = regExp2Ndfa' p n
        (NdfaT vq qq sq zq dq , nq) = regExp2Ndfa' q np
        v' = vp ++ vq 
        s' = sp ++ sq
        z' = zp ++ zq
        q' = qp ++ qq
        delta' = dp ++ dq 

regExp2Ndfa' (Star p) n = ( NdfaT v' q' s' z' delta' , np )
  where (NdfaT vp qp sp zp dp , np) = regExp2Ndfa' p (n)
        v' = vp 
        s' = sp
        z' = zp `union` sp
        q' = qp
        delta'= joinFinals dp zp sp

regExp2Ndfa' (OneOrMore e) n = regExp2Ndfa' (Then e (Star e)) n
--Podia-se fazer melhor: na de cima e na de baixo
regExp2Ndfa' (Optional e) n = regExp2Ndfa' (Or Epsilon e) n


joinFinals :: Eq st => [((st, Maybe sy), [st])] -> [st] -> [st] -> [((st, Maybe sy), [st])]
joinFinals [] finals inits = [((x,Nothing),inits) | x <- finals]
joinFinals (((x,Nothing),y):ds) finals inits 
       | x `elem` finals = ((x,Nothing),y++inits):joinFinals ds (delete x finals) inits 
       | otherwise = ((x,Nothing),y):joinFinals ds finals inits
joinFinals (d:ds) finals inits = d:joinFinals ds finals inits



regexpInt = ((Literal '-') `Or` (Literal '+') `Or` Epsilon) `Then` digitos `Then` (Star digitos)

