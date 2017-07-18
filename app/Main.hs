module Main where

import Lib

import Linear.Affine
import Linear.V3

csvFromPoint :: Point V3 Double -> String
csvFromPoint (P (V3 x y z)) =
  show x ++ "," ++ show y ++ "," ++ show z

csvFromBodies :: [Body] -> [String]
csvFromBodies [] = []
csvFromBodies (x:xs) =
  (bodyName x ++ ","
    ++ (show $ bodyMass x) ++ ","
    ++ (csvFromPoint $ bodyPosition x) ++ "\n")
  :(csvFromBodies xs)
  
steps :: Int -> Double -> [Body] -> IO ()
steps 0 _ _ = return ()
steps n dt bodies = do
  putStr . concat $ map ((++) (show n ++ ",")) $ csvFromBodies bodies
  steps (n-1) dt (updateAll dt bodies)

main :: IO ()
main = steps 1000000 10 [sun, earth, moon]
