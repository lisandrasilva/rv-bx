module RegExp2Ndfa where

import Data.List
import RegExp
import Dfa
import Ndfa
import Ndfa2Dfa


regExp2Ndfa :: RegExp -> Ndfa Int Char
regExp2Ndfa er = fst (regExp2Ndfa' er 1)

regExp2Ndfa' Empty n = ( Ndfa [] [sa,za] [sa] [za] delta , n+2 )
    where sa = n 
          za = n+1
          delta _ _ = []

regExp2Ndfa' (Epsilon) n = ( Ndfa [] [sa] [sa] [sa] delta , n+1 )
    where sa = n 
          delta _ _ = []

regExp2Ndfa' (Literal l) n = ( Ndfa [l] [sa,za] [sa] [za] delta , n+2)
  where sa = n 
        za = n+1
        delta q (Just a) | q == sa && l == a = [za] 
        delta _ _ = []

regExp2Ndfa' (Then p q) n = ( Ndfa v' q' s' z' delta' , nq)
  where (Ndfa vp qp sp zp dp , np) = regExp2Ndfa' p n
        (Ndfa vq qq sq zq dq , nq) = regExp2Ndfa' q np
        v' = vp `union` vq
        q' = qp `union` qq
        s' = sp
        z' = zq
        delta' q | q `elem` qp = if q `elem` zp then dp' q
                                                else dp  q 
                 | otherwise   = dq q
         where dp' q Nothing = (dp q Nothing) `union` sq
               dp' q (Just a) = dp q (Just a)

regExp2Ndfa' (Or p q) n = ( Ndfa v' q' s' z' delta' , nq+1 )
  where (Ndfa vp qp sp zp dp , np) = regExp2Ndfa' p (n + 1)
        (Ndfa vq qq sq zq dq , nq) = regExp2Ndfa' q np
        v' = vp `union` vq 
        s' = [n]
        z' = [nq]
        q' = s' `union` qp `union` qq `union` z'
        delta' q | q `elem` s' = dd q
                 | q `elem` zp = ddp' q
                 | q `elem` zq = ddq' q
                 | q `elem` qp = dp q
                 | q `elem` qq = dq q  
                 | otherwise   = dd'' q
         where dd q  Nothing = sp `union` sq
               dd _ _        = [] 

               ddp' q Nothing = z' `union` (dp q Nothing)
               ddp' _ _       = []

               ddq' q Nothing = z' `union` (dq q Nothing)
               ddq' _ _       = []

               dd'' _ _ = []

regExp2Ndfa' (Star p) n = ( Ndfa v' q' s' z' delta' , np+1 )
  where (Ndfa vp qp sp zp dp , np) = regExp2Ndfa' p (n + 1)
        v' = vp 
        s' = [n]
        z' = [np]
        q' = s'  `union` qp `union` z'
        delta' q | q `elem` s' = dd q
                 | q `elem` zp = dd' q
                 | otherwise   = dp q
          where dd q Nothing = sp `union` z'
                dd _ _ = []
                dd' q Nothing = sp `union` z'
                dd' _ _ = []

regexpInt = ((Literal '-') `Or` (Literal '+') `Or` Epsilon) `Then` digitos `Then` (Star digitos)

ndfaInt = Ndfa asciiTable [1,2,3,4,5,6,7] [1] [5] delta
            where   delta 1 (Just '+') = [2]
                    delta 1 (Just '-') = [3]
                    delta 1 Nothing    = [4]
                    delta 1 _          = [7]
                    delta 2 Nothing    = [4]
                    delta 2 _          = [7]
                    delta 3 Nothing    = [4]
                    delta 3 _          = [7]
                    delta 4 sy         = case sy of 
                                        Just x -> if x `elem` ['0'..'9'] then [5]
                                                  else [7]
                                        Nothing -> [7]
                    delta 5 Nothing    = [4]
                    delta _ _ = [7]

dfaInt = Dfa asciiTable [1,2,3,4,5] 1 [4] delta
            where   delta 1 '+' = 2
                    delta 1 '-' = 3
                    delta _ sy  = if sy `elem` ['0'..'9'] then 4
                                  else 5




