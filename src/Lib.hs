module Lib (
  gravity,
  Body,
  bodyDistance,
  field,
  acceleration,
  update,
  b1, b2, b3
  ) where

import Linear.Vector
import Linear.V3
import Linear.Affine
import Linear.Metric

--------------------------------------------------------------------------------
-- CONSTANTS
--------------------------------------------------------------------------------

-- Gravitational constant [m^3 kg^-1 s^-2]
gravity :: Double
gravity = 6.67408e-11

--------------------------------------------------------------------------------
-- BODY TYPE
--------------------------------------------------------------------------------

-- Body
data Body = Body {
  bodyMass :: Double,
  bodyPosition :: Point V3 Double,
  bodySpeed :: V3 Double
  } deriving (Show, Eq)


-- Distance between two bodies
bodyDistance :: Body -> Body -> Double
bodyDistance body1 body2 =
  distance (bodyPosition body1) (bodyPosition body2)


--------------------------------------------------------------------------------
-- GRAVITY FORCE
--------------------------------------------------------------------------------

-- Field created by a body on a certain position
field :: Body -> Point V3 Double -> V3 Double
field body pos =
  unP $ (gravity * m / r**2) *^ (normalize vec)
  where m = bodyMass body
        vec = (bodyPosition body) - pos
        r = norm vec

-- Acceleration given to a body by its neighbours
acceleration :: Body -> [Body] -> V3 Double
acceleration body = foldr f (fromInteger 0)
  where f neighbour acc =
          acc + field neighbour (bodyPosition body)


--------------------------------------------------------------------------------
-- SIMULATION
--------------------------------------------------------------------------------

-- Update speed and position with using a timestep dt
update :: Double -> Body -> [Body] -> Body
update dt (Body m pos speed) neighbours =
  Body m newpos newspeed
  where accel = acceleration (Body m pos speed) neighbours
        newspeed = speed + dt *^ accel
        newpos = pos + dt *^ P newspeed


--------------------------------------------------------------------------------
-- EXAMPLES
--------------------------------------------------------------------------------

b1 = Body 42 (P $ V3 0 0 0) (V3 0 0 0)
b2 = Body 11 (P $ V3 1 2 3) (V3 0 0 0)
b3 = Body 5 (P $ V3 5 2 1) (V3 0 0 0)

