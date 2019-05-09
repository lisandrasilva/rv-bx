module Interpreter where

import Data.List
import Data.Char
import System.Process
import System.IO
import Ndfa
import Dfa
import Ndfa2Dfa
import RegExp
import RegExp2Aut
import ShowAut
import AuxiliaryTypes
import Bx
import Examples

{- Given a string returns the converted Indexed Char (inverse of show) -}
readIChar :: String -> Error (Indexed Char)
readIChar (c:cs) | all isDigit cs = return  (I c (read cs))
readIChar x = fail ("Invalid index "++x)


{- Reads a list of states and produces the list of Indexed Char 
   Ex: readIChars "a2,a4" = [I 'a' 2 , I 'a' 4]
-}
readIChars :: String -> Error [Indexed Char]
readIChars s = rIChars (splitOn (\c -> c==',' || isSpace c) s)
       where rIChars :: [String] -> Error [Indexed Char]
             rIChars [] = return []
             rIChars (i:is) = do -- Error Monad
                                 ii  <- readIChar i
                                 iis <- rIChars is
                                 return (ii:iis)


splitOn :: (a->Bool) -> [a] -> [[a]]
splitOn f [] = []
splitOn f l = let (_,l') = span f l
                  (h,lt) = break f l'
              in if null h then [] else h:(splitOn f lt)


data Command = AddTransition [Indexed Char] [Indexed Char] Char
             | RemoveTransition [Indexed Char] [Indexed Char] Char 
             | RemState [Indexed Char]
             | Put
             | End deriving Show

readCommand :: String -> Error Command
--commands are of the form addT [a1 a3] b [b2 b4 b5]
--                      or remT [a1 a3] b [b2 b4 b5]
--                      or remS [a1 a3]
readCommand c = do let (com,args) = firstWord c
                   let as = (splitOn (\x -> (x=='[') || (x==']')) args)
                   case com of
                      "addT" -> do read3Args AddTransition as 
                      "remT" -> do read3Args RemoveTransition as
                      "remS" -> do st <- readIChars (head as)
                                   return (RemState st)
                      "put" -> return Put 
                      "quit" -> return End
                      _     -> fail ("Invalid command: " ++ com)
read3Args f args = do let argc = length args
                      if argc < 3 then fail "Too few arguments"
                      else if argc > 3 then fail "Too many arguments"
                      else do let [o,l,d] = args
                              e1 <- readIChars o
                              e2 <- readIChars d
                              c  <- readLabel  (dropWhile isSpace l)
                              return (f e1 e2 c)


--firstWord ss = break isSpace (snd (span isSpace ss))
firstWord ss = let ss1 = dropWhile isSpace ss
                   (w,ws) = break isSpace ss1
               in (w,dropWhile isSpace ws)


readLabel (c:_) = return c
readLabel _ = fail "Missing transition label"


readCom :: IO Command
readCom = do hSetBuffering stdin LineBuffering
             line <- getLine
             case readCommand line of
                Ok c -> return c
                Error err -> (putStrLn err) >> readCom


menuBX :: RegExp -> IO (Ndfa (Indexed Char) Char)
menuBX e = do putStrLn ("Expression     " ++ (show e) ++ "\n\n")
              menucycle nfa dfa
     where nfa  = glushkov e
           dfa  = nonDet2Det nfa



menucycle :: (Ndfa (Indexed Char) Char) 
        -> (Dfa [Indexed Char] Char) 
        -> IO (Ndfa (Indexed Char) Char)
menucycle nfa dfa = do putStrLn $ unlines $ help
                       displayFADot nfaDotFile nfa
                       displayFADot dfaDotFile dfa
                       com <- readCom
                       putStrLn ("\nCommand: "  ++ (show com))
                       res <- executeCommand com nfa dfa
                       case res of
                          Left (Error msg)  -> (putStrLn ("Error:  " ++ msg))
                                                  >> menucycle nfa dfa
                          Left (Ok (nfa',dfa')) -> menucycle nfa' dfa'
                          Right r -> return r
    where help = ["\n\n\n\n\n\n\n\n\n\n"
                 ,"Available commands are:"
                 ,"     . addT state label state"
                 ,"     . remT state label state"
                 ,"     . remS state"
                 ,"     . put"
                 ,"     . quit" , ""
                 ,"Where states are lists of symbols and a label is a single character"
                 , "Example:  addT [a1 a2] c [c1]"
                 ]


dfaDotFile = "_dfa.dot"
nfaDotFile = "_nfa.dot"
     
displayFADot file aut = do writeFile file (fa2Dot aut)
                           system ("dot -Tpng -O " ++ file)
                           system ("open " ++ file ++ ".png")


executeCommand :: Command 
               -> (Ndfa (Indexed Char) Char) 
               -> (Dfa [Indexed Char] Char) 
               -> IO (Either 
                         (Error (Ndfa (Indexed Char) Char, Dfa [Indexed Char] Char))
                         (Ndfa (Indexed Char) Char) )
executeCommand c ndfa dfa 
     = case c of 
         AddTransition source dest label 
            -> case dfaAddTrans ((source,label),dest) dfa of
                 Error s -> return $ Left (Error s)
                 Ok dfa' -> return $ Left (Ok (ndfa,dfa'))
         RemoveTransition source dest label 
            -> case dfaRemTrans ((source,label),dest) dfa of
                 Error s -> return $ Left (Error s)
                 Ok dfa' -> return $Left (Ok (ndfa,dfa'))
         RemState state 
            -> case dfaRemState state dfa of
                 Error s -> return $ Left (Error s)
                 Ok dfa' -> return $ Left (Ok (ndfa,dfa'))
         Put -> case putNdfa ndfa dfa of
                 Error s  -> return $ Left (Error s)
                 Ok ndfa' ->  let dfa' = nonDet2Det ndfa'
                              in if dfa' == dfa then return $ Left (Ok (ndfa',dfa'))
                                 else do putStrLn "View is not consistent with the source. Continue (y/n)"
                                         x <- getLine
                                         if x == "y" then return $ Left (Ok (ndfa',dfa'))
                                         else return $ Left (Ok (ndfa, nonDet2Det ndfa))
         End -> return $ Right ndfa


------

dfaAddTrans, dfaRemTrans :: (([Indexed Char], Char),[Indexed Char]) 
                         -> (Dfa [Indexed Char] Char)
                         -> Error (Dfa [Indexed Char] Char)
dfaAddTrans t@((s,l),d) (Dfa voc stat ini fin delta) =
       if (not $ allEqual s) then Error "Invalid transition origin: different characters"
       else if (not $ allEqual d) then Error "Invalid transition destination: different characters"
       else if l /= getSymbol (head d) then Error ("Invalid symbol ("++[l] ++"to that state")
       else case lookup (s,l) delta of
             Just _ -> Error "There exists a transition with that label from that state"
             Nothing -> Ok (Dfa (voc `union` [l]) (stat `union` [s,d]) ini fin (delta `union` [t]) )


dfaRemTrans t@((s,l),d) (Dfa voc stat ini fin delta) 
   = if not (t `elem` delta) then Error "Transition does not exist"
     else Ok (Dfa voc stat ini fin (delete t delta))

allEqual :: [Indexed Char] -> Bool
allEqual ((I x nx):(I y ny):xys) = x==y && allEqual ((I y ny):xys)
allEqual _ = True

dfaRemState :: [Indexed Char]
            -> (Dfa [Indexed Char] Char) 
            -> Error (Dfa [Indexed Char] Char)
dfaRemState state (Dfa voc stat ini fin delta) 
   = if not (state `elem` stat) then Error "State does not exist"
     else if state == ini then Error "Cannot remove the initial state"
     else Ok (Dfa voc (delete state stat) ini fin delta')
    where delta' = [((o,l),d) | ((o,l),d) <- delta, o /= state, d /= state]
