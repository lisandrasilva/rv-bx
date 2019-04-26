module RegExp2Aut where

import Data.List
import AuxiliaryTypes
import RegExp
import Ndfa


-- Glushkov Algorithm --- 
-- https://en.wikipedia.org/wiki/Glushkov%27s_construction_algorithm

-- First a generalization of regular expressions 
-- to allow an arbitrary vocabulary (and not only characters)
data RE a =  EMPTY
          |  EPSILON
          |  LITERAL a
          |  OR (RE a) (RE a)
          |  THEN (RE a) (RE a)
          |  STAR (RE a)
          |  ONEORMORE (RE a)
          |  OPTIONAL (RE a)
         deriving Eq

{- Definition of catamorphism for the Data Type RE a-}
cataRE :: t             -- Empty
       -> t             -- Epsilon
       -> (a -> t)      -- Literal Char
       -> (t -> t -> t) -- Or RegExp RegExp
       -> (t -> t -> t) -- Then RegExp RegExp
       -> (t -> t)      -- Star RegExp
       -> (t -> t)      -- OneOrMore RegExp
       -> (t -> t)      -- Optional RegExp
       -> RE a
       -> t
cataRE rx re fl fo ft fs fm fp EMPTY         = rx
cataRE rx re fl fo ft fs fm fp EPSILON       = re
cataRE rx re fl fo ft fs fm fp (LITERAL c)   = fl c
cataRE rx re fl fo ft fs fm fp (OR e1 e2)    = fo (cataRE rx re fl fo ft fs fm fp e1) 
                                                  (cataRE rx re fl fo ft fs fm fp e2)
cataRE rx re fl fo ft fs fm fp (THEN e1 e2)  = ft (cataRE rx re fl fo ft fs fm fp e1) 
                                                  (cataRE rx re fl fo ft fs fm fp e2)
cataRE rx re fl fo ft fs fm fp (STAR e)      = fs (cataRE rx re fl fo ft fs fm fp e)
cataRE rx re fl fo ft fs fm fp (ONEORMORE e) = fm (cataRE rx re fl fo ft fs fm fp e)
cataRE rx re fl fo ft fs fm fp (OPTIONAL e)  = fp (cataRE rx re fl fo ft fs fm fp e)


{- Conversion between RegExp and RE Char and back-}
regExp2RE :: RegExp -> RE Char
regExp2RE = cataRegExp EMPTY EPSILON LITERAL OR THEN STAR ONEORMORE OPTIONAL

re2RegExp :: RE Char -> RegExp
re2RegExp = cataRE Empty Epsilon Literal Or Then Star OneOrMore Optional

{- Inductive definition of show -}
instance Show a => Show (RE a) where
     show = cataRE "_" 
                   "@" 
                   show 
                   (\e f -> "(" ++ e ++ "|" ++ f ++ ")")
                   (\e f -> "(" ++ e ++ f ++ ")")
                   (\e -> "(" ++ e ++ ")*")
                   (\e -> "(" ++ e ++ ")+")
                   (\e -> "(" ++ e ++ ")?")


-- Glushkov Algorithm: phase 1 -- linearization
-- Linearization as in the wikipedia page 
glushkov_phase1 :: Eq a => RE a -> RE (Indexed a)
glushkov_phase1 =  fst . (glushkov_phase1_state [])

glushkov_phase1_state :: Eq a => [Indexed a] -> RE a -> (RE (Indexed a),[Indexed a])
glushkov_phase1_state state EMPTY = (EMPTY, state)
glushkov_phase1_state state EPSILON = (EPSILON, state)
glushkov_phase1_state state (LITERAL c) = 
      let n = 1+length state
      in (LITERAL (I c n), (I c n):state)
glushkov_phase1_state state (OR e1 e2) = 
      let (ee1,state' ) = glushkov_phase1_state state  e1
          (ee2,state'') = glushkov_phase1_state state' e2
      in (OR ee1 ee2, state'')
glushkov_phase1_state state (THEN e1 e2) = 
      let (ee1,state' ) = glushkov_phase1_state state  e1
          (ee2,state'') = glushkov_phase1_state state' e2
      in (THEN ee1 ee2, state'')
glushkov_phase1_state state (STAR e) = 
      let (ee,state') = glushkov_phase1_state state e
      in (STAR ee, state')
glushkov_phase1_state state (ONEORMORE e) = 
      let (ee,state') = glushkov_phase1_state state e
      in (ONEORMORE ee, state')
glushkov_phase1_state state (OPTIONAL e) = 
      let (ee,state') = glushkov_phase1_state state e
      in (OPTIONAL ee, state')


{- Glushkov Algorithm: phase 2
    Given a linear regular expression e' compute (P(e'),D(e'),F(e'),L(e')) where
      - P(e'), the set of letters which occurs as first letter of a word 
      - D(e'), the set of letters which can end a word 
      - F(e'), the set of letter pairs that can occur in words
      - L(e'), True iff the empty word belongs to the language
-}
glushkov_phase2 :: Eq a => RE a -> ([a],[a],[(a,a)],Bool) 
glushkov_phase2 = cataRE ([],[],[],False) -- EMPTY
                         ([],[],[],True)  -- EPSILON
                         (\x -> ([x],[x],[],False)) -- LITERAL x
                         (\(px,dx,tx,ex) (py,dy,ty,ey) -> (px ++ py, dx ++ dy, tx ++ ty, ex || ey)) -- OR x y
                         (\(px,dx,tx,ex) (py,dy,ty,ey) -> (if ex then (px ++ py) else px,
                                                           if ey then (dx ++ dy) else dy,
                                                           (tx ++ ty ++ [(x,y)| x<-dx,y<-py]),
                                                           ex && ey)) -- THEN x y
                         (\(px,dx,tx,ex) -> (px,dx,(tx `union` [(x,y)| x<-dx,y<-px]),True)) -- STAR x
                         (\(px,dx,tx,ex) -> (px,dx,(tx `union` [(x,y)| x<-dx,y<-px]),False)) -- ONEORMORE x
                         (\(px,dx,tx,ex) -> (px,dx,tx,True)) -- OPTIONAL x

-- Putting it all together
glushkov :: RegExp -> Ndfa (Indexed Char) Char
glushkov e = let (le,states) = glushkov_phase1_state [] (regExp2RE e)
                 (starts, ends, transitions, emp) = glushkov_phase2 le
                 ini = (I '_' 0)
                 finals = if emp then (ini:ends) else ends
                 delta = [((ini,c),(I c x)) | (I c x) <- starts] 
                       ++ [((x,y),(I y n)) | (x, (I y n)) <- transitions]
                 vocabulary = nub (map getSymbol states)
             in Ndfa vocabulary (ini:states) [ini] finals delta

mapNdfa :: (sa -> sb) -> (va -> vb) -> Ndfa sa va -> Ndfa sb vb
mapNdfa fs fv (Ndfa voc sta ini fin delta) = (Ndfa voc' sta' ini' fin' delta')
     where voc' = map fv voc
           sta' = map fs sta
           ini' = map fs ini
           fin' = map fs fin
           delta' = [((fs o, fv l), fs ds) | ((o,l),ds) <- delta]


{- Conversion between a Regular Expression and a Ndfa using 
   Thompson's algorithm 
   https://en.wikipedia.org/wiki/Thompson%27s_construction 
   Properties of the produced automaton:
      has exactly ONE initial state
      has exactly ONE final state
      the number of transitions from any state is at most TWO
-}

thompson :: RegExp -> Ndfa Int (Eps Char)
thompson er = fst (regExp2Ndfa' er 1)

regExp2Ndfa' :: RegExp -> Int -> (Ndfa Int (Eps Char),Int)
-- regExp2Ndfa e n = (a,p) where a is an automaton with states n..(p-1)
regExp2Ndfa' Empty n = ( Ndfa [] [sa,za] [sa] [za] delta , n+2 )
    where sa = n
          za = n+1
          delta = []

regExp2Ndfa' (Epsilon) n = ( Ndfa [] [sa,za] [sa] [za] delta , n+2 )
    where sa = n 
          za = n+1
          delta = [((sa,Eps),za)]

regExp2Ndfa' (Literal l) n = ( Ndfa [Symb l] [sa,za] [sa] [za] delta , n+2)
  where sa = n
        za = n+1
        delta=[((sa,Symb l),za)]
regExp2Ndfa' (Then p q) n = ( Ndfa v' q' s' z' delta' , nq)
  where (Ndfa vp qp [sp] [zp] dp , np) = regExp2Ndfa' p n
        (Ndfa vq qq [sq] [zq] dq , nq) = regExp2Ndfa' q np
        v' = vp ++ vq
        q' = qp ++ qq
        s' = [sp]
        z' = [zq]
        delta' = ((zp,Eps),sq):(dp ++ dq)
regExp2Ndfa' (Or p q) n = ( Ndfa v' q' [s'] [z'] delta' , (nq+1))
  where (Ndfa vp qp [sp] [zp] dp , np) = regExp2Ndfa' p (n+1)
        (Ndfa vq qq [sq] [zq] dq , nq) = regExp2Ndfa' q np
        v' = vp ++ vq 
        q' = [s',z'] ++ qp ++ qq
        s' = n
        z' = nq
        delta' = [((s',Eps),sp),((zp,Eps),z')] ++ dp ++ dq 

regExp2Ndfa' (Star p) n = ( Ndfa v' q' [s'] [z'] delta' , (np+1) )
  where (Ndfa vp qp [sp] [zp] dp , np) = regExp2Ndfa' p (n+1)
        v' = vp 
        q' = qp
        s' = n
        z' = np
        delta'= [((s',Eps),sp), ((zp,Eps),z')
                ,((zp,Eps),sp), ((s',Eps),z')]
                ++ dp 
regExp2Ndfa' (OneOrMore e) n = regExp2Ndfa' (Then e (Star e)) n
regExp2Ndfa' (Optional e) n = regExp2Ndfa' (Or Epsilon e) n