module Route3 where

import Prelude hiding (Monoid, Semigroup, Functor, fmap, Applicative, (<*>), pure)

import Optional
import Monoid
import Functor
import Applicative

type Duration = Int
type Time = Int

data Operation = TrackIn | Process | TrackOut
  deriving Show

data Route op =
    Route (RouteRem op)
  | RouteQTLimit Time (Route op) (RouteRem op)
  deriving Show

instance Functor Route where
  fmap f (Route rr) = Route (fmap f rr)
  fmap f (RouteQTLimit t r rr) =
    RouteQTLimit t (fmap f r) (fmap f rr)
  
newtype RouteRem op = RouteRem [RouteElement op]
  deriving Show

instance Functor RouteRem where
  fmap f (RouteRem els) = RouteRem (fmap (fmap f) els)

instance Monoid (RouteRem op) where
  (RouteRem els1) `combine` (RouteRem els2) = RouteRem (combine els1 els2)
  neutral = RouteRem []

data RouteElement op =
    RouteOp op
  | RouteQTZone Duration (RouteRem op)
  deriving Show

instance Functor RouteElement where
  fmap f (RouteOp op) = RouteOp (f op)
  fmap f (RouteQTZone d rr) = RouteQTZone d (fmap f rr)

-- nonono - do it on RouteRem
instance Applicative RouteElement where
 pure x = RouteOp x
 (RouteOp f) <*> (RouteOp x) = RouteOp (f x)
 (RouteOp f) <*> (RouteQTZone d rr) = RouteQTZone d (fmap f rr)
 (RouteQTZone d rr) <*> (RouteOp x) = undefined

-- monad instance
joinRouteRem :: RouteRem (RouteRem op) -> RouteRem op
joinRouteRem (RouteRem elements) = RouteRem (concat (fmap joinRouteElement elements))

joinRouteElement :: RouteElement (RouteRem op) -> [RouteElement op]
joinRouteElement (RouteOp (RouteRem elements)) = elements
joinRouteElement (RouteQTZone d rr) =
  [RouteQTZone d (joinRouteRem rr)]

r1 = Route (RouteRem [RouteOp TrackIn, RouteOp Process, RouteOp Process, RouteOp TrackOut])
r2 = Route (RouteRem [RouteOp TrackIn,
       RouteQTZone 5 (RouteRem [RouteOp Process, RouteOp Process]),
       RouteOp TrackOut])
r3 = Route (RouteRem [RouteOp TrackIn,
       RouteQTZone 5
       (RouteRem [RouteOp Process,
         RouteQTZone 7
         (RouteRem [RouteOp Process, RouteOp Process])])])

routeHead :: Route op -> Optional op
routeHead (Route rem) = routeRemHead rem
routeHead (RouteQTLimit _t rt rm) =
  case routeHead rt of
    Absent -> routeRemHead rm
    Present h -> Present h

routeRemHead :: RouteRem op -> Optional op
routeRemHead (RouteRem []) = Absent
routeRemHead (RouteRem (el:rest)) =
  case routeElementHead el of
    Absent -> routeRemHead (RouteRem rest)
    Present op -> Present op

routeElementHead :: RouteElement op -> Optional op
routeElementHead (RouteOp op) = Present op
routeElementHead (RouteQTZone _d rm) = routeRemHead rm

{-
    Pattern match(es) are non-exhaustive
    In an equation for ‘routeAdvance’:
        Patterns not matched: ((RouteQTZone _ _:_))
-}

routeAdvance :: Route op -> Time ->  Optional (op, Route op)
routeAdvance (Route rm) t = routeRemAdvance rm t
routeAdvance (RouteQTLimit _lt rt rm) t =
  case routeAdvance rt t of
    Absent -> routeRemAdvance rm t
    Present (op, rt') -> Present (op, RouteQTLimit t rt' rm)

routeRemAdvance (RouteRem []) _t = Absent
routeRemAdvance (RouteRem ((RouteOp op):rest)) _t = Present (op, Route (RouteRem rest))
routeRemAdvance (RouteRem ((RouteQTZone d rm):rest)) t =
  routeAdvance (RouteQTLimit (t+d) (Route rm) (RouteRem rest)) t
