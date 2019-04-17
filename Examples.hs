module Examples where

import RegExp

litA  = Literal 'a'
litB  = Literal 'b'
litC  = Literal 'c'

zeroAsMoreBs :: RegExp
zeroAsMoreBs =  Then  (Star litA)
                      (Then litB (Star litB))

zeroAsMoreBs' = Then (Star litA) (OneOrMore litB)

asOubs = Then (Or litA litB) (Star (Or litA litB))

asOubs' = OneOrMore (litA `Or` litB)


digitos =  (Literal '0') `Or` (Literal '1') `Or` (Literal '2') `Or`
           (Literal '3') `Or` (Literal '4') `Or` (Literal '5') `Or`
           (Literal '6') `Or` (Literal '7') `Or` (Literal '8') `Or`
           (Literal '9')
digs = foldr1 Or (map Literal ['0'..'9'])


intDenotation =  ((Literal '-') `Or` (Literal '+') `Or` Epsilon)
                 `Then` (digitos `Then` (Star digitos))


realDenotation =  ((Literal '-') `Or` (Literal '+') `Or` Epsilon)
                  `Then` (Star digitos) `Then` ((Literal '.') `Or` Epsilon)
                  `Then` digitos `Then` (Star digitos)

isInt :: String -> Bool
isInt = matches intDenotation


-- Example
-- b \ ((d + i);)^* \  e
{-
nd1 = NdfaT "bdi;e" [1,2,3,4,5] [1] [5] delta
        where delta= delta1
delta1 = [((1,'b'),[2])
         ,((2,'d'),[3])
         ,((2,'i'),[3])
         ,((2,'e'),[5])
         ,((3,';'),[4])
         ,((4,'@'),[2])
         ]



er1 = Or (Star (Literal 'a')) (Star (Literal 'b'))

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
-}


-- Examples 

-- example from the wikipedia page
-- e = (a(ab)^*)^* + (ba)^*

exGlushkov = (Star ( (Literal 'a') `Then` (Star ((Literal 'a') `Then` (Literal 'b')))))
             `Or`
             (Star ((Literal 'b') `Then` (Literal 'a')))

{-
exGlu = regExp2RE exGlushkov

exGluPh1 = glushkov_phase1 exGlu


exGluPh2 = glushkov_phase2 exGluPh1


teste = glushkov_phase2 . glushkov_phase1 . regExp2RE
fromString = (foldr1 Then) . (map Literal)


--   *RegExp2Aut> exGluPh2
--   ([('a',1),('b',4)]
--   ,[('a',1),('b',3),('a',5)]
--   ,[[('a',2),('b',3)]
--    ,[('b',3),('a',2)]
--    ,[('a',1),('a',2)]
--    ,[('a',1),('a',1)]
--    ,[('b',3),('a',1)]
--    ,[('b',4),('a',5)]
--    ,[('a',5),('b',4)]]
--   ,True)
-}

regexpInt = ((Literal '-') `Or` (Literal '+') `Or` Epsilon) `Then` digitos `Then` (Star digitos)

er1 = Or (Then (Literal 'a') (Star (Literal 'a'))) (Then (Literal 'a') (Star (Literal 'b')))

-- a?a?b*

er2 = Then (Optional (Literal 'a')) (Then (Optional (Literal 'a')) (Star (Literal 'b')))