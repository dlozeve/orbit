module Main where

import Lib

import Linear.Affine
import Linear.V3

import System.Random
import Control.Monad (replicateM)

import Graphics.Gloss hiding (Point)

import Options.Applicative
import Data.Semigroup ((<>))
import Safe

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

--------------------------------------------------------------------------------
-- Commandline arguments
--------------------------------------------------------------------------------

data CmdLineOptions = CmdLineOptions
  { optionNumberBodies :: Int
  , optionColor :: Color
  , optionFPS :: Int
  }

numberBodiesParser :: Parser Int
numberBodiesParser = option auto
  (long "number-bodies"
   <> short 'n'
   <> help "Number of Bodies"
   <> showDefault
   <> value 300
   <> metavar "N")

fpsParser :: Parser Int
fpsParser = option auto
  (long "fps"
   <> help "Frame rate"
   <> showDefault
   <> value 25
   <> metavar "FPS")

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
                "" -> []
                s' -> w : splitOn c s''
                  where (w, s'') = break (== c) s'

colorParser :: Parser Color
colorParser = option (eitherReader readRGB)
  (long "color"
   <> short 'c'
   <> help "Foreground color RGBA (0-255)"
   <> showDefault
   <> value white
   <> metavar "R,G,B")
  where readRGB  s =
          case mapM readEitherSafe $ splitOn ',' s of
            Right (r:g:b:a:_) -> Right $ makeColorI r g b a
            Right (r:g:b:_) -> Right $ makeColorI r g b 255
            Right (r:g:_) -> Right $ makeColorI r g 255 255
            Right (r:_) -> Right $ makeColorI r 255 255 255
            Right  _  -> Right $ makeColorI 255 255 255 255
            Left s -> Left s

optionsParser :: Parser CmdLineOptions
optionsParser = CmdLineOptions <$>
  numberBodiesParser <*> colorParser <*> fpsParser

opts :: ParserInfo CmdLineOptions
opts = info (optionsParser <**> helper)
  (fullDesc
   <> progDesc "Barnes-Hut N-body simulator"
   <> header "orbit")

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
displayBody b = translate (realToFrac x) (realToFrac y) $ circleSolid (realToFrac (_bodyRadius b))
  where P (V3 x y _) = _bodyPosition b

displayBodies :: Color -> [Body] -> Picture
displayBodies c = color c . Pictures . map displayBody

main :: IO ()
main = do
  CmdLineOptions n c fps <- execParser opts
  bodies <- replicateM n randomBody
  simulate
    window
    black
    fps
    bodies
    (displayBodies c)
    (\_ dt bs -> updateAll (realToFrac dt*1e6) 0.5 bs)
