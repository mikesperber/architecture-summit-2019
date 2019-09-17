{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Route1 where

import Optional
  
type Duration = Int
type Time = Int

data Operation = TrackIn | Process | TrackOut 

deriving instance Show Operation

type Route = [RouteElement]

data RouteElement where
  RouteOp :: Operation -> RouteElement
  RouteQTZone :: Duration -> Route -> RouteElement
  -- probem here: can appear anywhere
  RouteQTLimit :: Time -> Route -> RouteElement

deriving instance Show RouteElement

r1 = [RouteOp TrackIn, RouteOp Process, RouteOp Process, RouteOp TrackOut]
r2 = [RouteOp TrackIn,
       RouteQTZone 5 [RouteOp Process, RouteOp Process],
       RouteOp TrackOut]
r3 = [RouteOp TrackIn,
       RouteQTZone 5
       [RouteOp Process,
         RouteQTZone 7
         [RouteOp Process, RouteOp Process]]]

routeHead :: Route -> Optional Operation
routeHead [] = Absent
routeHead (el:rest) =
  case routeElementHead el of
    Absent -> routeHead rest
    Present op -> Present op

routeElementHead :: RouteElement -> Optional Operation
routeElementHead (RouteOp op) = Present op
routeElementHead (RouteQTZone _ rt) = routeHead rt

{-
    Pattern match(es) are non-exhaustive
    In an equation for ‘routeAdvance’:
        Patterns not matched: ((RouteQTZone _ _:_))
-}

routeAdvance :: Route -> Time ->  Optional (Operation, Route)
routeAdvance [] t = Absent
routeAdvance (el:rest) t =
  case el of
    RouteOp op -> Present (op, rest)
    RouteQTZone d rt -> 
      routeAdvance (RouteQTLimit (t + d) rt : rest) t
    RouteQTLimit tl rt  ->
      case routeAdvance rt t of
        Absent -> routeAdvance (rest) t
        Present (op, rt') -> Present (op, RouteQTLimit tl rt' : rest)

r4 = [RouteOp TrackIn,
      RouteQTLimit 12
      [RouteOp Process, RouteOp TrackOut]]
     
{-
routeAdvance (RouteQTLimit tm rm after) t =
  case routeAdvance rm t of
    Absent -> routeAdvance after t
    Present (op, rest) -> Present (op, RouteQTLimit tm rest after)
-}

{-
routeEnterQTZone :: Route -> Time -> Route
routeEnterQTZone rt tm =
  r rt (\route -> route)
  where
    r :: Route -> (Route -> Route) -> Route
    r (((RouteQTZone dur rm):rest)) f =
      r rm (\body -> RouteQTLimit (tm + dur) body (f (rest)))
    r (_) f = f rt
    r (RouteQTLimit end rm rest) f =
       r rm (\body -> RouteQTLimit end body (f rest))
    

-}
