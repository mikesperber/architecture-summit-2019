{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
module Route2 where

import Optional

type Duration = Int
type Time = Int

data Operation = TrackIn | Process | TrackOut 

deriving instance Show Operation

data Route where
  Route :: RouteRem -> Route
  RouteQTLimit :: Time -> Route -> RouteRem -> Route

deriving instance Show Route
  
type RouteRem = [RouteElement]

data RouteElement where
  RouteOp :: Operation -> RouteElement
  RouteQTZone :: Duration -> RouteRem -> RouteElement

deriving instance Show RouteElement

r1 = Route [RouteOp TrackIn, RouteOp Process, RouteOp Process, RouteOp TrackOut]
r2 = Route [RouteOp TrackIn,
       RouteQTZone 5 ([RouteOp Process, RouteOp Process]),
       RouteOp TrackOut]
r3 = Route [RouteOp TrackIn,
       RouteQTZone 5
       [RouteOp Process,
         RouteQTZone 7
         [RouteOp Process, RouteOp Process]]]

routeHead :: Route -> Optional Operation
routeHead (Route rem) = routeRemHead rem
routeHead (RouteQTLimit _t rt rm) =
  case routeHead rt of
    Absent -> routeRemHead rm
    Present h -> Present h

routeRemHead :: RouteRem -> Optional Operation
routeRemHead [] = Absent
routeRemHead (el:rest) =
  case routeElementHead el of
    Absent -> routeRemHead rest
    Present op -> Present op

routeElementHead :: RouteElement -> Optional Operation
routeElementHead (RouteOp op) = Present op
routeElementHead (RouteQTZone _d rm) = routeRemHead rm

{-
    Pattern match(es) are non-exhaustive
    In an equation for ‘routeAdvance’:
        Patterns not matched: ((RouteQTZone _ _:_))
-}

routeAdvance :: Route -> Time ->  Optional (Operation, Route)
routeAdvance (Route rm) t = routeRemAdvance rm t
routeAdvance (RouteQTLimit _lt rt rm) t =
  case routeAdvance rt t of
    Absent -> routeRemAdvance rm t
    Present (op, rt') -> Present (op, RouteQTLimit t rt' rm)

routeRemAdvance [] _t = Absent
routeRemAdvance (el:rest) t =
  case el of
    RouteOp op -> Present (op, Route rest)
    RouteQTZone d rm ->
      routeAdvance (RouteQTLimit (t+d) (Route rm) rest) t

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
