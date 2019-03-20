import Data.Char
-- Basic types
-- Values 

--- Page 4 

-- Event name
type Name = String

-- Events 
type Event v = (Name, [v])

-- Traces
type Trace v = [Event v]

-- Veredict 
-- the return value of monitors
-- can be, for instance, Bool

type Veredict = Bool

-- Properties 
type Property v d = Trace v -> d


--- Page 5

-- Monitors

-- Monitor = < D , A , Q , q0 , delta , tau >
--             |   |   |    |     |      |
--             |   |   |    |     |      veredict function :: Q -> D
--             |   |   |    |     Monitor state transition :: (Q,A) -> Q
--             |   |   |    Initial monitor state
--             |   |   Set of monitor states
--             |   Possible events
--             Veredicts

data Monitor d a q = Monitor { initial :: q
                             , delta :: (q,a) -> q
                             , veredict :: q -> d
                             }
-- to test
mon1 :: Monitor Bool Int Int
mon1 = Monitor 0 (\(x,y)->x+y) odd

runMonitor :: Monitor d a q -> [a] -> [d]
runMonitor m [] = [veredict m (initial m)]
runMonitor m (e:es) = (veredict m i):runMonitor (m{initial=delta m (i,e)}) es
    where i = initial m

-- Runtime verification System

-- RV = < D , A , P , G >
--        |   |   |   | 
--        |   |   |   Monitor generating :: P -> Monitor D A
--        |   |   set of properties :: Property B D (for some B subset A)
--        |   Events
--        Veredicts

data RV d v q = RV { events :: Event v
                   , properties :: [Property v d]
                   , monitorG :: [Property v d] -> Monitor d v q
                   }

--- Page 6

-- Symbolic Events

type SymbEvent x v = (Name, [Either x v])

type Binding x v = [(x,v)] -- could be modelled with x->v

eventMatch :: (Eq v, Eq x) => SymbEvent x v -> Event v -> Maybe (Binding x v)
eventMatch (e1,ls) (e2,l) | e1 == e2 = symbMatch [] ls l
                          | otherwise = Nothing
 where symbMatch m [] [] = Just m
       symbMatch m ((Left var):ex) (ev:events)
            = case lookup var m of
                Nothing -> symbMatch ((var,ev):m) ex events
                Just ev' -> if ev' == ev then symbMatch m ex events 
                                         else Nothing
       symbMatch m ((Right ev):ex) (ev':events)
            = if ev' == ev then symbMatch m ex events 
                           else Nothing

-- two events to test the above
readSymbInt :: String  -> [Either String Int]
-- converts a string of numbers and non-numbers (vars) into list of Either
-- readSymbInt "x 2 z" == [Left "x", Right 2,  Left "z"]
readSymbInt text = map decide (words text)
    where decide w | all isDigit w = Right (read w)
                   | otherwise = Left w 

event1 :: Event Int
event1 = ("A", [1,2,3,1,2,3])

sEvent1, sEvent2 :: SymbEvent String Int
sEvent1 = ("A", readSymbInt "x y z x y 3")
sEvent2 = ("A", readSymbInt "x 2 z x y 3")

--- Page 7

-- Rover System

data Task = Task { id :: Int 
                 , method :: RoverMethod
                 } deriving Eq

data RoverMethod = SendCommand String Int
                 | SendGrant Resource
                 | SendRescind Resource
    deriving Eq

type Resource = Int

data Scheduler = LogSuccess String Int
               | LogFailure String Int
               | SetConflict Resource Resource
               | SetPriority Resource Resource
               | SendRequest Task Resource
               | SendCancel Task Resource
     deriving Eq

-- Just for fun
instance Show Task where
   show (Task i t) = "Task " ++ (show i) ++ ": " ++ show t

instance Show RoverMethod where
  show (SendCommand s i) = "command " ++ s ++ " (" ++ show i ++ ")"
  show (SendGrant r)     = "grant   " ++ show r
  show (SendRescind r)   = "rescind " ++ show r

instance Show Scheduler where
  show (LogSuccess s i)    = "log: " ++ s ++ " (" ++ show i ++ ") signalled success"
  show (LogFailure s i)    = "log: " ++ s ++ " (" ++ show i ++ ") signalled failure"
  show (SetConflict r1 r2) = "conflicting resources " ++ show r1 ++ " and " ++ show r2
  show (SetPriority r1 r2) = "resource " ++ show r1 ++ " has higher priority than " ++ show r2
  show (SendRequest t r)   = "resource " ++ show r ++ " requested by " ++ show t
  show (SendCancel t r)    = "resource " ++ show r ++ " cancelled by " ++ show t
  showList = (++) . unlines . (map show)

-- Example of Figure 3 (page 8)

-- the scheduler commands a task (task1) to perform a job
sh1 = SendRequest (Task 1 (SendCommand "Do something" 1)) r1
r1 = 1
r2 = 2
sh2 = SendRequest (Task 2 (SendGrant r2)) r2
-- task1 requests a resource r1
sh3 = SendRequest (Task 1 (SendGrant r1)) r1
-- which has higher priority 
--   than a conflicting resource r2
sh4 = SetConflict r1 r2
--     already granted to another task (task2)
-- task2 is therefore asked to rescind r2
sh5 = SendRequest (Task 2 (SendRescind r2)) r2
--   which it does 
--     after which resource r1 is granted to task1
sh6 = SendRequest (Task 1 (SendGrant r1)) r1
-- which after completed 
--   hands back the resource r1
sh7 = SendRequest (Task 1 (SendRescind r1)) r1
--     and reports success 
sh8 = LogSuccess "Completed" 1

figure3 :: [Scheduler]
figure3 = [ sh1, sh2, sh3, sh4, sh5, sh6, sh7, sh8]
