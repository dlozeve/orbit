module Main where

import Lib

import Linear.Affine
import Linear.V3

import System.Random
import Control.Monad (replicateM)

import Graphics.Gloss hiding (Point)


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
  -- Make radius proportional to mass for visualization
  let radius = 20 * m
  -- Scale mass
  let mass = 1e3 * m
  -- Scale position and speed
  let posx = 1e3 * (2*x - 1)
  let posy = 1e3 * (2*y - 1)
  let speedx = 5e-5 * vx
  let speedy = 5e-5 * vy
  return $ Body "random" radius mass (P $ V3 posx posy 0) (V3 speedx speedy 0)


--------------------------------------------------------------------------------
-- CSV export
--------------------------------------------------------------------------------

csvFromPoint :: Point V3 Double -> String
csvFromPoint (P (V3 x y z)) =
  show x ++ "," ++ show y ++ "," ++ show z

csvFromBodies :: [Body] -> [String]
csvFromBodies =
  map (\ x ->
         bodyName x ++
        "," ++
        show (bodyMass x) ++
        "," ++ csvFromPoint (bodyPosition x) ++ "\n")
  
steps :: Int -> Double -> [Body] -> IO ()
steps 0 _ _ = return ()
steps n dt bodies = do
  putStr . concat $ map ((show n ++ ",") ++) $ csvFromBodies bodies
  steps (n-1) dt (updateAll dt bodies)


--------------------------------------------------------------------------------
-- Gloss
--------------------------------------------------------------------------------

width, height, offset :: Int
width = 1000
height = 750
offset = 100

window :: Display
window = InWindow "Orbit" (width, height) (offset, offset)

displayBody :: Body -> Picture
displayBody b = translate (realToFrac x) (realToFrac y) $ circle (realToFrac (bodyRadius b))
  where P (V3 x y _) = bodyPosition b

displayBodies :: [Body] -> Picture
displayBodies = color white . Pictures . map displayBody

drawing :: Picture
drawing = color white $ circle 80

main :: IO ()
main = do
  bodies <- replicateM 300 randomBody
  simulate
    window
    black
    25
    bodies
    displayBodies
    (\_ dt bs -> updateAll (realToFrac dt*1e6) bs)

--main :: IO ()
--main = steps 1000000 10 [sun, earth, moon, mercury, venus, mars]

