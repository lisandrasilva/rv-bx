module Examples where

import RegExp
import AuxiliaryTypes

-- a?a?b*
re :: RegExp
re = Then (Optional (Literal 'a')) (Then (Optional (Literal 'a')) (Star (Literal 'b')))

litA  = Literal 'a'
litB  = Literal 'b'
litC  = Literal 'c'

zeroAsMoreBs :: RegExp
zeroAsMoreBs =  Then  (Star litA)
                      (Then litB (Star litB))

zeroAsMoreBs' = Then (Star litA) (OneOrMore litB)

asOrbs = Then (Or litA litB) (Star (Or litA litB))

asOrbs' = OneOrMore (litA `Or` litB)


digits =  (Literal '0') `Or` (Literal '1') `Or` (Literal '2') `Or`
           (Literal '3') `Or` (Literal '4') `Or` (Literal '5') `Or`
           (Literal '6') `Or` (Literal '7') `Or` (Literal '8') `Or`
           (Literal '9')

digs = foldr1 Or (map Literal ['0'..'9'])


intDenotation =  ((Literal '-') `Or` (Literal '+') `Or` Epsilon)
                 `Then` (digits `Then` (Star digits))


realDenotation =  ((Literal '-') `Or` (Literal '+') `Or` Epsilon)
                  `Then` (Star digits) `Then` ((Literal '.') `Or` Epsilon)
                  `Then` digits `Then` (Star digits)

isInt :: String -> Bool
isInt = matches intDenotation


-- Examples 

-- example from the wikipedia page
-- e = (a(ab)^*)^* + (ba)^*
exGlushkov = (Star ( (Literal 'a') `Then` (Star ((Literal 'a') `Then` (Literal 'b')))))
             `Or`
             (Star ((Literal 'b') `Then` (Literal 'a')))

regexpInt = ((Literal '-') `Or` (Literal '+') `Or` Epsilon) `Then` digits `Then` (Star digits)

re1 = Or (Then (Literal 'a') (Star (Literal 'a'))) (Then (Literal 'a') (Star (Literal 'b')))

