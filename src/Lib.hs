module Lib (
  gravity,
  Body,
  bodyName,
  bodyMass,
  bodyPosition,
  bodySpeed,
  bodyDistance,
  field,
  acceleration,
  update,
  updateAll,
  b1, b2, b3,
  au, sun, earth, moon
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
  bodyName :: String,
  bodyMass :: Double, -- [kg]
  bodyPosition :: Point V3 Double, -- [m]
  bodySpeed :: V3 Double -- [m/s]
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
update dt (Body name m pos speed) neighbours =
  Body name m newpos newspeed
  where accel = acceleration (Body name m pos speed) neighbours
        newspeed = speed + dt *^ accel
        newpos = pos + dt *^ P newspeed

-- Update all bodies with a timestep dt
updateAll :: Double -> [Body] -> [Body]
updateAll dt bodies = aux [] [] bodies
  where
    -- Cycles through all bodies, updates each one and stores it in
    -- res. Each body already updated is moved to prev.
    aux res prev [] = res
    aux res prev (b:next) =
          aux (newb:res) (b:prev) next
          where newb = update dt b $ prev ++ next
                    
--------------------------------------------------------------------------------
-- EXAMPLES
--------------------------------------------------------------------------------

b1 = Body "b1" 42e12 (P $ V3 0 0 0) (V3 0 0 0)
b2 = Body "b2" 11e12 (P $ V3 1 2 3) (V3 0 3e3 0)
b3 = Body "b3" 5e12 (P $ V3 5 2 1) (V3 3e3 1e3 0)

-- Astronomical Unit [m]
au :: Double
au = 149597870700

sun  = Body "Sun" 1.98855e30 (P $ V3 0 0 0) (V3 0 0 0)
earth = Body "Earth" 8.97237e24 (P $ V3 au 0 0) (V3 0 29.78e3 0)
moon = Body "Moon" 7.342e22 (P $ V3 (au+384399e3) 0 0) (V3 0 (29.78e3+1022) 0)
