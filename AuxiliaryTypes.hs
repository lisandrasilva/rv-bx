module AuxiliaryTypes where

data Indexed a = I a Int deriving (Eq,Ord)
getSymbol (I a _) = a
getIndex (I _ i) = i

findI :: (Eq a) => a -> [Indexed a] -> Maybe Int
findI _ [] = Nothing
findI x ((I c n):ys) = if x == c then Just n else findI x ys

instance Show a => Show (Indexed a) where
   show (I a i) = filter (not.((`elem` "\"\'"))) ((show a) ++ (show i))


-- The type Eps a is isomorphic to Maybe a and is used here for display 
-- reasons, i.e. to have a special instance of the Show class

data Eps a = Eps | Symb a deriving (Eq,Ord)

instance Show a => Show (Eps a) where
    show Eps = "@"
    show (Symb x) = show x

injEps :: [a] -> [Eps a]
injEps = map Symb

catEps ::[Eps a] -> [a]
catEps [] = []
catEps ((Symb x):xs) = x:catEps xs
catEps (Eps:xs) = catEps xs


noRepetitions :: (Eq a) => [a] -> Bool
noRepetitions [] = True
noRepetitions [x] = True
noRepetitions (h:t) = not (h `elem` t) && noRepetitions t
