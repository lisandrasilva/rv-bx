module RegExp where


data RegExp  =  Empty
             |  Epsilon
             |  Literal Char
             |  Or RegExp RegExp
             |  Then RegExp RegExp
             |  Star RegExp
             |  OneOrMore RegExp
             |  Optional RegExp
         deriving Eq

-- cataRegExp :: a -> a -> (Char -> a) -> (a->a->a)... -> RegExp -> a
cataRegExp rx re fl fo ft fs fm fp Empty = rx
cataRegExp rx re fl fo ft fs fm fp Epsilon = re
cataRegExp rx re fl fo ft fs fm fp (Literal c)  = fl c
cataRegExp rx re fl fo ft fs fm fp (Or e1 e2)   = fo (cataRegExp rx re fl fo ft fs fm fp e1) (cataRegExp rx re fl fo ft fs fm fp e2)
cataRegExp rx re fl fo ft fs fm fp (Then e1 e2) = ft (cataRegExp rx re fl fo ft fs fm fp e1) (cataRegExp rx re fl fo ft fs fm fp e2)
cataRegExp rx re fl fo ft fs fm fp (Star e) = fs (cataRegExp rx re fl fo ft fs fm fp e)
cataRegExp rx re fl fo ft fs fm fp (OneOrMore e) = fm (cataRegExp rx re fl fo ft fs fm fp e)
cataRegExp rx re fl fo ft fs fm fp (Optional e) = fp (cataRegExp rx re fl fo ft fs fm fp e)

instance Show RegExp where
  show = cataRegExp "{}"
                    "@"
                    (\a -> [a]) -- (:[])
                    (\e f -> "(" ++ e ++ "|" ++ f ++ ")")
                    (\e f -> "(" ++ e ++ f ++ ")")
                    (\e -> "(" ++ e ++ ")*")
                    (\e -> "(" ++ e ++ ")+")
                    (\e -> "(" ++ e ++ ")?")

zeroAsMoreBs :: RegExp
zeroAsMoreBs =  Then  (Star (Literal 'a'))
                      (Then (Literal 'b') (Star (Literal 'b')))

a  = Literal 'a'
b  = Literal 'b'
c  = Literal 'c'

zeroAsMoreBs' = Then (Star a) (OneOrMore b)

asOubs = Then (Or a b) (Star (Or a b))

asOubs' = OneOrMore (a `Or` b)


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



showRE :: RegExp -> [Char]
showRE Empty           = "{}"
showRE Epsilon         = "@"
showRE (Literal a)     = [a]
showRE (Or re1 re2)    = "(" ++ (showRE re1) ++ "|" ++ (showRE re2) ++ ")"
showRE (Then re1 re2)  = "(" ++ (showRE re1) ++ (showRE re2) ++ ")"
showRE (Star re)       = "(" ++ (showRE re) ++ ")*"
showRE (OneOrMore re)  = "(" ++ (showRE re) ++ ")+"
showRE (Optional re)   = "(" ++ (showRE re) ++ ")?"


matches :: RegExp -> String -> Bool
matches Empty inp           = False
matches Epsilon inp         = inp==[]
matches (Literal l) inp     = inp==[l]
matches (Or re1 re2) inp    = matches re1 inp || matches re2 inp
matches (Then re1 re2) inp  = or [ matches re1 s1 && matches re2 s2
                                 | (s1,s2) <- splits inp ]
matches (Star re) inp       =  matches Epsilon inp ||
                               or  [ matches re s1 && matches (Star re) s2 
                                   | (s1,s2) <- frontSplits inp ]

splits         :: [a] -> [ ([a],[a]) ]
splits s       =  [ splitAt n s | n <- [ 0 .. length s ] ]

frontSplits    :: [a] -> [ ([a],[a]) ]
frontSplits s  =  [ splitAt n s | n <- [ 1 .. length s ] ]

isInt :: String -> Bool
isInt = matches intDenotation


matches' = matches . extREtoRE

extREtoRE Empty           = Empty
extREtoRE Epsilon         = Epsilon
extREtoRE (Literal a)     = (Literal a)
extREtoRE (Or re1 re2)    = Or (extREtoRE re1) (extREtoRE re2)
extREtoRE (Then re1 re2)  = Then (extREtoRE re1) (extREtoRE re2)
extREtoRE (Star re)       = Star (extREtoRE re)
extREtoRE (OneOrMore re)  = (extREtoRE re) `Then` (Star (extREtoRE re))
extREtoRE (Optional re)   = (extREtoRE re) `Or` Epsilon

extREtoREcata = cataRegExp Empty Epsilon Literal Or Then Star (\e -> e `Then` (Star e)) (\e-> e `Or` Epsilon)


finite :: RegExp -> Bool
finite (Star re) = True
finite (OneOrMore re) = True
finite (Or re1 re2) = finite re1 || finite re2
finite (Then re1 re2) = finite re1 || finite re2
finite _ = False


size :: RegExp -> Int 
size Empty = 0
size Epsilon = 1
size (Literal a) = 1
size (Or re1 re2) = size re1 + size re2
size (Then re1 re2) = size re1 + size re2
size (Star re) = size re
size (OneOrMore re) = size re
size (Optional re) = size re

sizecata = cataRegExp 0 1 (\x -> 1) (+) (+) id id id






