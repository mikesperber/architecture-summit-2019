module Route0 where

import Optional

type Duration = Int
type Time = Int

data Operation = TrackIn | Process | TrackOut 
  deriving Show

type Route = [Operation]

r1 = [TrackIn, Process, Process, TrackOut]

routeHead :: Route -> Optional Operation
routeHead [] = Absent
routeHead (op:_) = Present op

routeAdvance :: Route -> Optional (Operation, Route)
routeAdvance [] = Absent
routeAdvance (op:rest) = Present (op, rest)

