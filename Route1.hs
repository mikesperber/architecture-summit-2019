module Route1 where

import Optional
  
type Duration = Int
type Time = Int

data Operation = TrackIn | Process | TrackOut 
  deriving Show

type Route = [RouteElement]

data RouteElement =
    RouteOp Operation
  | RouteQTZone Duration Route
  -- problem here: can appear anywhere
  | RouteQTLimit Time Route
  deriving Show

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
