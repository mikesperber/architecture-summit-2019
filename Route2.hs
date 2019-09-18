module Route2 where

import Prelude hiding (Monoid, Semigroup, Functor, fmap, Applicative, (<*>), pure)

import Optional
import Monoid

type Duration = Int
type Time = Int

data Operation = TrackIn | Process | TrackOut 
  deriving Show

data Route =
    Route RouteRem
  | RouteQTLimit Time Route RouteRem
  deriving Show
  
type RouteRem = [RouteElement]

data RouteElement =
    RouteOp Operation
  | RouteQTZone Duration RouteRem
  deriving Show

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
routeAdvance (RouteQTLimit lt rt rm) t =
  case routeAdvance rt t of
    Absent -> routeRemAdvance rm t
    Present (op, rt') -> Present (op, RouteQTLimit t rt' rm)

routeRemAdvance [] _t = Absent
routeRemAdvance (el:rest) t =
  case el of
    RouteOp op -> Present (op, Route rest)
    RouteQTZone d rm ->
      routeAdvance (RouteQTLimit (t+d) (Route rm) rest) t

-- monoid instance for RouteRem: need newtype
{-
instance Monoid RouteRem where
  (RouteRem els1) `combine` (RouteRem els2) = RouteRem (combine els1 els2)
  neutral = RouteRem []
-}

