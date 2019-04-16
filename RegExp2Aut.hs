module RegExp2Aut where

import Data.List
import RegExp
import Dfa
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
data Indexed a = I a Int deriving (Eq,Ord)
noindex (I a _) = a

findI :: (Eq a) => a -> [Indexed a] -> Maybe Int
findI _ [] = Nothing
findI x ((I c n):ys) = if x == c then Just n else findI x ys

instance Show a => Show (Indexed a) where
   show (I a i) = filter (not.((`elem` "\"\'"))) ((show a) ++ (show i))

-- Linearization as in the wikipedia page 
glushkov_phase1 :: Eq a => RE a -> RE (Indexed a)
glushkov_phase1 =  fst . (glushkov_phase1_state [])

glushkov_phase1_state :: Eq a => [Indexed a] -> RE a -> (RE (Indexed a),[Indexed a])
glushkov_phase1_state state EMPTY = (EMPTY, state)
glushkov_phase1_state state EPSILON = (EPSILON, state)
glushkov_phase1_state state (LITERAL c) = 
      let n = length state
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

{- Alternative version where the counter is local for each symbol
glushkov_phase1_alt :: Eq a => RE a -> RE (Indexed a)
glushkov_phase1_alt =  fst . (glushkov_phase1_state_alt [])

glushkov_phase1_state_alt :: Eq a => [(Indexed a)] 
                              -> RE a 
                              -> (RE (Indexed a),[(Indexed a)])
glushkov_phase1_state_alt state EMPTY = (EMPTY, state)
glushkov_phase1_state_alt state EPSILON = (EPSILON, state)
glushkov_phase1_state_alt state (LITERAL c) = 
      case findI c state of
              Just n -> (LITERAL (I c (n+1)), (I c (n+1)):(state))
              Nothing -> (LITERAL (I c 1), (I c 1):state)
glushkov_phase1_state_alt state (OR e1 e2) = 
      let (ee1,state' ) = glushkov_phase1_state_alt state  e1
          (ee2,state'') = glushkov_phase1_state_alt state' e2
      in (OR ee1 ee2, state'')
glushkov_phase1_state_alt state (THEN e1 e2) = 
      let (ee1,state' ) = glushkov_phase1_state_alt state  e1
          (ee2,state'') = glushkov_phase1_state_alt state' e2
      in (THEN ee1 ee2, state'')
glushkov_phase1_state_alt state (STAR e) = 
      let (ee,state') = glushkov_phase1_state_alt state e
      in (STAR ee, state')
glushkov_phase1_state_alt state (ONEORMORE e) = 
      let (ee,state') = glushkov_phase1_state_alt state e
      in (ONEORMORE ee, state')
glushkov_phase1_state_alt state (OPTIONAL e) = 
      let (ee,state') = glushkov_phase1_state_alt state e
      in (OPTIONAL ee, state')

-}


{- Glushkov Algorithm: phase 2
    Given a linear regular expression e' compute (P(e'),D(e'),F(e'),L(e')) where
      - P(e'), the set of letters which occurs as first letter of a word 
      - D(e'), the set of letters which can end a word 
      - F(e'), the set of letter pairs that can occur in words
      - L(e'), True iff the empty word belongs to the language
-}
glushkov_phase2 :: Eq a => RE a -> ([a],[a],[[a]],Bool) 
glushkov_phase2 = cataRE ([],[],[],False) -- EMPTY
                         ([],[],[],True)  -- EPSILON
                         (\x -> ([x],[x],[],False)) -- LITERAL x
                         (\(px,dx,tx,ex) (py,dy,ty,ey) -> (px `union` py, dx `union` dy, tx `union` ty, ex || ey)) -- OR x y
                         (\(px,dx,tx,ex) (py,dy,ty,ey) -> (if ex then (px `union` py) else px,
                                                           if ey then (dx `union` dy) else dy,
                                                           nub (tx ++ ty ++ [[x,y]| x<-dx,y<-py]),
                                                           ex && ey)) -- THEN x y
                         (\(px,dx,tx,ex) -> (px,dx,nub (tx ++[[x,y]| x<-dx,y<-px]),True)) -- STAR x
                         (\(px,dx,tx,ex) -> (px,dx,nub (tx ++[[x,y]| x<-dx,y<-px]),False)) -- ONEORMORE x
                         (\(px,dx,tx,ex) -> (px,dx,tx,True)) -- OPTIONAL x

-- Putting it all together
glushkov :: RegExp -> NdfaT (Indexed Char) Char
glushkov e = let (le,states) = glushkov_phase1_state [] (regExp2RE e)
                 (starts, ends, transitions, emp) = glushkov_phase2 le
                 ini = (I '_' 0)
                 finals = if emp then (ini:ends) else ends
                 delta = [((ini,c),[(I c x)]) | (I c x) <- starts] 
                         ++ grouping [ (((I x nx),y),(I y ny))| [(I x nx),(I y ny)] <- transitions]
                 vocabulary = nub (map noindex states)
             in NdfaT vocabulary states [ini] finals delta



{- Conversion between a Regular Expression and a Ndfa using Thompson's algorithm -}
{-
thompson :: RegExp -> NdfaT Int Char
thompson er = fst (regExp2Ndfa' er 1)

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
        delta=[((n,l),[n+1])]


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
-}



