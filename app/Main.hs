module Main where

import Lib

import Linear.Affine
import Linear.V3

import System.Random
import Control.Monad (replicateM)


--------------------------------------------------------------------------------
-- Random body generation
--------------------------------------------------------------------------------

randomBody :: IO Body
randomBody = do
  r <- randomIO :: IO Double
  m <- randomIO :: IO Double
  x <- randomIO :: IO Double
  y <- randomIO :: IO Double
  z <- randomIO :: IO Double
  vx <- randomIO :: IO Double
  vy <- randomIO :: IO Double
  vz <- randomIO :: IO Double
  name <- replicateM 20 $ randomRIO ('a', 'z')
  -- Make radius proportional to mass for visualization
  let radius = 20 * m
  -- Scale mass
  let mass = 1e3 * m
  -- Scale position and speed
  let posx = 1e3 * (2*x - 1)
  let posy = 1e3 * (2*y - 1)
  let speedx = 5e-5 * vx
  let speedy = 5e-5 * vy
  return $ Body name radius mass (P $ V3 posx posy 0) (V3 speedx speedy 0)


--------------------------------------------------------------------------------
-- CSV export
--------------------------------------------------------------------------------

-- | Show a Vector as CSV
csvFromVector :: V3 Double -> String
csvFromVector (V3 x y z) =
  show x ++ "," ++ show y ++ "," ++ show z

-- | show a Point as CSV
csvFromPoint :: Point V3 Double -> String
csvFromPoint (P v) = csvFromVector v

-- | Show a Body as CSV
csvFromBody :: Double -> Body -> String
csvFromBody dt b =
  show dt ++ "," ++
  csvFromPoint (_bodyPosition b) ++ "," ++
  csvFromVector (_bodySpeed b) ++ "\n"

-- | Show a list of bodies as CSV
csvFromBodies :: Double -> [Body] -> String
csvFromBodies dt bs = concat $ map (csvFromBody dt) bs

-- | Compute all the steps of the simulation
steps :: Double -- ^ The time step
  -> Double -- ^ The Barnes-Hut threshold theta
  -> [Body] -- ^ The initial state (list of bodies)
  -> [(Double, [Body])] -- ^ List of successive states with the
                        -- corresponding time
steps dt theta b = zip (iterate (dt +) 0) (iterate (updateAll dt theta) b)

-- | Show all the steps as CSV
csvFromInit :: Int -- ^ The number of time steps to keep
  -> Double -- ^ The time step
  -> Double -- ^ The Barnes-Hut threshold theta
  -> [Body] -- ^ The initial state (list of bodies)
  -> String -- ^ CSV data
csvFromInit n dt theta b = concat $ map (uncurry csvFromBodies) (take n $ steps dt theta b)

main :: IO ()
main = do
  bodies <- replicateM 100 randomBody
  putStrLn $ csvFromInit 10000 (60*20) 0.5 bodies

