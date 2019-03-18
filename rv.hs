-- Basic types
-- Values 

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










