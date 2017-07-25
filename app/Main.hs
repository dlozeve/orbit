module Main where

import Lib

import Linear.Affine
import Linear.V3

import Graphics.Gloss hiding (Point)


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
displayBody b = translate (realToFrac x/1e9) (realToFrac y/1e9) $ circle (realToFrac (bodyRadius b)/1e8)
  where P (V3 x y _) = bodyPosition b

displayBodies :: [Body] -> Picture
displayBodies = color white . Pictures . map displayBody

drawing :: Picture
drawing = color white $ circle 80

main :: IO ()
main = simulate
  window
  black
  25
  [sun, earth, moon, mercury, venus, mars]
  displayBodies
  (\_ dt bs -> updateAll (realToFrac dt*1e6) bs)

--main :: IO ()
--main = steps 1000000 10 [sun, earth, moon, mercury, venus, mars]

