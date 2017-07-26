{-|
Module      : Lib
Description : N-body simulation
Copyright   : (c) Dimitri Lozeve, 2017
License     : BSD3
Maintainer  : dimitri.lozeve@gmail.com
-}
module Lib (
  -- * Constants
  gravity,
  -- * Body type
  Body(..),
  bodyDistance,
  -- * Gravity force
  field,
  acceleration,
  -- * Simulation
  update,
  updateAll,
  -- * Examples
  au, sun, earth, moon, mercury, venus, mars
  ) where

import Linear.Vector
import Linear.V3
import Linear.Affine
import Linear.Metric

--------------------------------------------------------------------------------
-- CONSTANTS
--------------------------------------------------------------------------------

-- | Gravitational constant [m^3 kg^-1 s^-2]
gravity :: Double
gravity = 6.67408e-11

--------------------------------------------------------------------------------
-- BODY TYPE
--------------------------------------------------------------------------------

-- | Body type
data Body = Body {
  bodyName :: String, -- ^ Name
  bodyRadius :: Double, -- ^ Radius [m]
  bodyMass :: Double, -- ^ Mass [kg]
  bodyPosition :: Point V3 Double, -- ^ Position [m]
  bodySpeed :: V3 Double -- ^ Speed [m/s]
  } deriving (Show, Eq)


-- | Distance between two bodies
bodyDistance :: Body -> Body -> Double
bodyDistance body1 body2 =
  distance (bodyPosition body1) (bodyPosition body2)


--------------------------------------------------------------------------------
-- GRAVITY FORCE
--------------------------------------------------------------------------------

-- | Field created by a body on a certain position
field :: Body -> Point V3 Double -> V3 Double
field body pos =
  unP $ (gravity * m / r**2) *^ normalize vec
  where m = bodyMass body
        vec = bodyPosition body - pos
        r = norm vec

-- | Acceleration given to a body by its neighbours
acceleration :: Body -> [Body] -> V3 Double
acceleration body = foldr f 0
  where f neighbour acc =
          acc + field neighbour (bodyPosition body)


--------------------------------------------------------------------------------
-- SIMULATION
--------------------------------------------------------------------------------

-- | Update speed and position
update :: Double -- ^ The time step
  -> Body -- ^ The body to update
  -> [Body] -- ^ The Body's neighbours
  -> Body -- ^ The updated Body
update dt (Body name r m pos speed) neighbours =
  Body name r m newpos newspeed
  where accel = acceleration (Body name r m pos speed) neighbours
        newspeed = speed + dt *^ accel
        newpos = pos + dt *^ P newspeed

-- | Update all bodies with a timestep dt
updateAll :: Double -> [Body] -> [Body]
updateAll dt = aux [] []
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

-- | Astronomical Unit [m]
au :: Double
au = 149597870700

-- | The Sun
sun  = Body "Sun" 695700000 1.98855e30 (P $ V3 0 0 0) (V3 0 0 0)
-- | The Earth
earth = Body "Earth" 6.371e6  8.97237e24 (P $ V3 au 0 0) (V3 0 29.78e3 0)
-- | The Moon
moon = Body "Moon" 1.7371e6 7.342e22 (P $ V3 (au+384399e3) 0 0) (V3 0 (29.78e3+1022) 0)
-- | Mercury
mercury = Body "Mercury" 2.4397e6 3.3011e23 (P $ V3 57909050000 0 0) (V3 0 47362 0)
-- | Venus
venus = Body "Venus" 6.0518e6 4.8675e24 (P $ V3 108208000000 0 0) (V3 0 35.02e3 0)
-- | Mars
mars = Body "Mars" 3.3895e6 6.4171e23 (P $ V3 227.9392e9 0 0) (V3 0 24.077e3 0)
