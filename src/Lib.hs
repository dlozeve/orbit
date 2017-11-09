{-|
Module      : Lib
Description : N-body simulation
Copyright   : (c) Dimitri Lozeve, 2017
License     : BSD3
Maintainer  : dimitri.lozeve@gmail.com
-}

{-# LANGUAGE TemplateHaskell #-}

module Lib (
  -- * Constants
  gravity,
  -- * Body type
  Body(..),
  bodyDistance,
  -- * Barnes-Hut
  Region(..),
  Octree(..),
  Octant(..),
  selectOctant,
  subOctree,
  updateRegion,
  insertBody,
  buildTree,
  -- * Gravity force
  field,
  acceleration,
  -- * Simulation
  update,
  updateAll,
  -- * Examples
  au, sun, earth, moon, mercury, venus, mars
  ) where

import Data.List
import Data.Foldable

import Linear.Vector
import Linear.V3
import Linear.Affine
import Linear.Metric

import Control.Lens hiding (Empty)

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
  _bodyName :: String, -- ^ Name
  _bodyRadius :: Double, -- ^ Radius [m]
  _bodyMass :: Double, -- ^ Mass [kg]
  _bodyPosition :: Point V3 Double, -- ^ Position [m]
  _bodySpeed :: V3 Double -- ^ Speed [m/s]
  } deriving (Show, Eq)

makeLenses ''Body


-- | Distance between two bodies
bodyDistance :: Body -> Body -> Double
bodyDistance body1 body2 =
  distance (_bodyPosition body1) (_bodyPosition body2)


--------------------------------------------------------------------------------
-- BARNES-HUT
--------------------------------------------------------------------------------

-- | Region type, represents a region in space
data Region = Region {
  _regionCenter :: Point V3 Double,
  _regionCenterOfMass :: Point V3 Double,
  _regionMass :: Double,
  _regionDiameter :: Double
  } deriving (Show, Eq)

makeLenses ''Region

-- | Main data structure for the Octree
data Octree = Empty Region
            | Single Region Body
            | Node Region Octree Octree Octree Octree Octree Octree Octree Octree
  deriving (Show, Eq)

-- | One of the 8 octants from a given reference point
data Octant = NED | NWD | SWD | SED
            | NEU | NWU | SWU | SEU
  deriving (Show, Eq)

-- | Return the octant in which is located a body
selectOctant :: Point V3 Double -> Body -> Octant
selectOctant center body = aux $ _bodyPosition body .-. center
  where aux (V3 x y z) = case (x > 0, y > 0, z > 0) of
          (True, True, True) -> NEU
          (True, True, False) -> NED
          (True, False, True) -> SEU
          (True, False, False) -> SED
          (False, True, True) -> NWU
          (False, True, False) -> NWD
          (False, False, True) -> SWU
          (False, False, False) -> SWD

-- | Create a subtree for a given Region and Octant
subOctree :: Region -> Octant -> Octree
subOctree r octant =
  Empty (r & regionDiameter .~ newdiameter
          & regionCenter %~ (+ ((newdiameter/2) *^ centershift)))
  where newdiameter = (r ^. regionDiameter) / 2
        centershift = case octant of
          NED -> P $ V3 1 1 (-1)
          NWD -> P $ V3 (-1) 1 (-1)
          SWD -> P $ V3 (-1) (-1) (-1)
          SED -> P $ V3 1 (-1) (-1)
          NEU -> P $ V3 1 1 1
          NWU -> P $ V3 (-1) 1 1
          SWU -> P $ V3 (-1) (-1) 1
          SEU -> P $ V3 1 (-1) 1

-- | Update the mass and the center of mass of a region when adding a
-- new Body
updateRegion :: Body -> Region -> Region
updateRegion b r =
  r & regionMass .~ newmass
  & regionCenterOfMass .~ newcenter
  where newmass = _regionMass r + _bodyMass b
        newcenter = ((_regionMass r *^ _regionCenterOfMass r) + (_bodyMass b *^ _bodyPosition b))
                    ^/ (_regionMass r + _bodyMass b)

-- | Insert a new body in an Octree
insertBody :: Body -> Octree -> Octree
insertBody b t = case t of
  -- If it is empty, we turn it into a singleton Region, adjusting its
  -- mass and center of mass. However, if the body is too far away
  -- (i.e. outside the diameter of the Region), we just ignore it.
  Empty r -> if distance (_bodyPosition b) (_regionCenter r) > (_regionDiameter r)
             then Empty r
             else Single (updateRegion b r) b
  -- If it is a singleton, we create the 8 subtrees and we insert the
  -- two bodies in them. We will end up in the Node case below.
  Single r b' ->
    insertBody b $ insertBody b' $
                 Node r (subOctree r NED) (subOctree r NWD) (subOctree r SWD) (subOctree r SED)
                 (subOctree r NEU) (subOctree r NWU) (subOctree r SWU) (subOctree r SEU)
  -- Finally, if it is already a tree, we have to choose in which
  -- octant inserting the new body.
  Node r ned nwd swd sed neu nwu swu seu ->
    let r' = updateRegion b r
    in
      case (selectOctant (r ^. regionCenter) b) of
        NED -> Node r' (insertBody b ned) nwd swd sed neu nwu swu seu
        NWD -> Node r' ned (insertBody b nwd) swd sed neu nwu swu seu
        SWD -> Node r' ned nwd (insertBody b swd) sed neu nwu swu seu
        SED -> Node r' ned nwd swd (insertBody b sed) neu nwu swu seu
        NEU -> Node r' ned nwd swd sed (insertBody b neu) nwu swu seu
        NWU -> Node r' ned nwd swd sed neu (insertBody b nwu) swu seu
        SWU -> Node r' ned nwd swd sed neu nwu (insertBody b swu) seu
        SEU -> Node r' ned nwd swd sed neu nwu swu (insertBody b seu)


-- | Build a Barnes-Hut Octree from a list of Bodies
buildTree :: [Body] -> Octree
buildTree [] = Empty (Region { _regionCenter = P $ V3 0 0 0,
                               _regionCenterOfMass = P $ V3 0 0 0,
                               _regionMass = 0,
                               _regionDiameter = 0
                             })
buildTree bs = foldr insertBody (Empty r) bs
  where r = Region { _regionCenter = center,
                     _regionCenterOfMass = center,
                     _regionMass = 0,
                     _regionDiameter = diameter
                   }
        -- We determine the initial center and diameter of the region
        -- using the positions of all input bodies.
        positions :: [Point V3 Double]
        positions = map (flip (^.) bodyPosition) bs
        -- The center is just the geometric center of all positions.
        center :: Point V3 Double
        center = (sum $ positions) ^/ (fromIntegral $ length positions)
        -- The diameter is the maximum range of the coordinates. This
        -- is roughly 2x more what is needed.
        diameter :: Double
        diameter = maximum $ map (\xs -> maximum xs - minimum xs) $ transpose $ map toList positions


--------------------------------------------------------------------------------
-- GRAVITY FORCE
--------------------------------------------------------------------------------

-- | Field created by a body on a certain position
field :: Body -> Point V3 Double -> V3 Double
field body pos =
  unP $ (gravity * m / r**2) *^ normalize vec
  where m = _bodyMass body
        vec = _bodyPosition body - pos
        r = norm vec

-- | Acceleration given to a body by its neighbours
acceleration :: Double -> Octree -> Body -> V3 Double
acceleration theta tree body = case tree of
  Empty _ -> V3 0 0 0
  Single _ b ->
    if _bodyPosition b == _bodyPosition body then V3 0 0 0
    else field b (_bodyPosition body)
  Node r ned nwd swd sed neu nwu swu seu ->
    let s = _regionDiameter r
        vec = _regionCenterOfMass r - _bodyPosition body
        d = norm vec in
      if s/d < theta then
        unP $ (gravity * _regionMass r / d**2) *^ normalize vec
      else
        acceleration theta ned body +
        acceleration theta nwd body +
        acceleration theta swd body +
        acceleration theta sed body +
        acceleration theta neu body +
        acceleration theta nwu body +
        acceleration theta swu body +
        acceleration theta seu body


--------------------------------------------------------------------------------
-- SIMULATION
--------------------------------------------------------------------------------

-- | Update speed and position
update :: Double -- ^ The time step
  -> Double -- ^ The theta threshold parameter for Barnes-Hut
  -> Octree -- ^ The Barnes-Hut Octree
  -> Body -- ^ The body to update
  -> Body -- ^ The updated Body
update dt theta tree (Body name r m pos speed) =
  Body name r m newpos newspeed
  where accel = acceleration theta tree (Body name r m pos speed)
        newspeed = speed + dt *^ accel
        newpos = pos + dt *^ P newspeed


-- | Update all Bodies with a time step dt and a Barnes-Hut threshold
-- theta
updateAll :: Double -- ^ The time step
  -> Double -- ^ The Barnes-Hut threshold theta
  -> [Body] -- ^ A list of Bodies
  -> [Body] -- ^ The updated list of Bodies
updateAll dt theta bs =
  map (update dt theta tree) bs
  where tree = buildTree bs



--------------------------------------------------------------------------------
-- EXAMPLES
--------------------------------------------------------------------------------

-- | Astronomical Unit [m]
astronomicalUnit :: Double
astronomicalUnit = 149597870700

-- | The Sun
sun  = Body "Sun" 695700000 1.98855e30 (P $ V3 0 0 0) (V3 0 0 0)
-- | The Earth
earth = Body "Earth" 6.371e6  8.97237e24 (P $ V3 astronomicalUnit 0 0) (V3 0 29.78e3 0)
-- | The Moon
moon = Body "Moon" 1.7371e6 7.342e22 (P $ V3 (astronomicalUnit+384399e3) 0 0) (V3 0 (29.78e3+1022) 0)
-- | Mercury
mercury = Body "Mercury" 2.4397e6 3.3011e23 (P $ V3 57909050000 0 0) (V3 0 47362 0)
-- | Venus
venus = Body "Venus" 6.0518e6 4.8675e24 (P $ V3 108208000000 0 0) (V3 0 35.02e3 0)
-- | Mars
mars = Body "Mars" 3.3895e6 6.4171e23 (P $ V3 227.9392e9 0 0) (V3 0 24.077e3 0)
