module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Linear.Vector
import Linear.V3
import Linear.Affine
import Linear.Metric

import Lib

main :: IO ()
main = do
  defaultMain (testGroup "Orbit tests" [unitTests, propertyChecks])

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "bodyDistance sun earth"
    $ bodyDistance sun earth @?= astronomicalUnit
  , testCase "bodyDistance earth moon"
    $ bodyDistance earth moon @?= 384399e3
  , testCase "selectOctant sun earth"
    $ selectOctant (_bodyPosition sun) earth @?= SED
  , testCase "subOctree for an Empty Region"
    $ let r = Region (P $ V3 0 0 0) (P $ V3 0 0 0) 1 4
          r' = Region (P $ V3 1 1 1) (P $ V3 0 0 0) 1 2
      in subOctree r NEU @?= Empty r'
  , testCase "buildTree []"
    $ let tree = Empty (Region {_regionCenter = P (V3 0.0 0.0 0.0), _regionCenterOfMass = P (V3 0.0 0.0 0.0), _regionMass = 0.0, _regionDiameter = 0.0})
      in buildTree [] @?= tree
  , testCase "buildTree [earth]"
    $ let tree = Single (Region {_regionCenter = P (V3 1.495978707e11 0.0 0.0), _regionCenterOfMass = P (V3 1.495978707e11 0.0 0.0), _regionMass = 8.97237e24, _regionDiameter = 0.0}) (Body {_bodyName = "Earth", _bodyRadius = 6371000.0, _bodyMass = 8.97237e24, _bodyPosition = P (V3 1.495978707e11 0.0 0.0), _bodySpeed = V3 0.0 29780.0 0.0})
    in buildTree [earth] @?= tree
  , testCase "buildTree [sun, earth]"
    $ let tree = Node (Region {_regionCenter = P (V3 7.479893535e10 0.0 0.0), _regionCenterOfMass = P (V3 1349963.8779584717 0.0 0.0), _regionMass = 1.98856794474e30, _regionDiameter = 1.495978707e11}) (Empty (Region {_regionCenter = P (V3 1.12198403025e11 3.7399467675e10 (-3.7399467675e10)), _regionCenterOfMass = P (V3 1.495978707e11 0.0 0.0), _regionMass = 8.97237e24, _regionDiameter = 7.479893535e10})) (Empty (Region {_regionCenter = P (V3 3.7399467675e10 3.7399467675e10 (-3.7399467675e10)), _regionCenterOfMass = P (V3 1.495978707e11 0.0 0.0), _regionMass = 8.97237e24, _regionDiameter = 7.479893535e10})) (Single (Region {_regionCenter = P (V3 3.7399467675e10 (-3.7399467675e10) (-3.7399467675e10)), _regionCenterOfMass = P (V3 674984.9844950008 0.0 0.0), _regionMass = 1.98855897237e30, _regionDiameter = 7.479893535e10}) (Body {_bodyName = "Sun", _bodyRadius = 6.957e8, _bodyMass = 1.98855e30, _bodyPosition = P (V3 0.0 0.0 0.0), _bodySpeed = V3 0.0 0.0 0.0})) (Single (Region {_regionCenter = P (V3 1.12198403025e11 (-3.7399467675e10) (-3.7399467675e10)), _regionCenterOfMass = P (V3 1.495978707e11 0.0 0.0), _regionMass = 1.794474e25, _regionDiameter = 7.479893535e10}) (Body {_bodyName = "Earth", _bodyRadius = 6371000.0, _bodyMass = 8.97237e24, _bodyPosition = P (V3 1.495978707e11 0.0 0.0), _bodySpeed = V3 0.0 29780.0 0.0})) (Empty (Region {_regionCenter = P (V3 1.12198403025e11 3.7399467675e10 3.7399467675e10), _regionCenterOfMass = P (V3 1.495978707e11 0.0 0.0), _regionMass = 8.97237e24, _regionDiameter = 7.479893535e10})) (Empty (Region {_regionCenter = P (V3 3.7399467675e10 3.7399467675e10 3.7399467675e10), _regionCenterOfMass = P (V3 1.495978707e11 0.0 0.0), _regionMass = 8.97237e24, _regionDiameter = 7.479893535e10})) (Empty (Region {_regionCenter = P (V3 3.7399467675e10 (-3.7399467675e10) 3.7399467675e10), _regionCenterOfMass = P (V3 1.495978707e11 0.0 0.0), _regionMass = 8.97237e24, _regionDiameter = 7.479893535e10})) (Empty (Region {_regionCenter = P (V3 1.12198403025e11 (-3.7399467675e10) 3.7399467675e10), _regionCenterOfMass = P (V3 1.495978707e11 0.0 0.0), _regionMass = 8.97237e24, _regionDiameter = 7.479893535e10}))
    in buildTree [sun, earth] @?= tree
  , testCase "updateAll 1 1 []"
    $ updateAll 1 1 [] @?= []
  , testCase "updateAll 0 0.5 [earth]"
    $ updateAll 0 0.5 [earth] @?= [earth]
  , testCase "isEmpty $ buildTree []"
    $ assertBool "buildTree [] is not Empty" (isEmpty $ buildTree [])
  ]

instance (Arbitrary a) => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary
  
instance (Arbitrary (f a)) => Arbitrary (Point f a) where
  arbitrary = P <$> arbitrary

instance Arbitrary Body where
  arbitrary = do
    name <- arbitrary
    Positive radius <- arbitrary
    Positive mass <- arbitrary
    pos <- arbitrary
    speed <- arbitrary
    return $ Body name radius mass pos speed

instance Arbitrary Region where
  arbitrary = do
    center <- arbitrary
    centerOfMass <- arbitrary
    Positive mass <- arbitrary
    Positive diameter <- arbitrary
    return $ Region center centerOfMass mass diameter

instance Arbitrary Octant where
  arbitrary = oneof $ map return [NED, NWD, SWD, SED, NEU, NWU, SWU, SEU]

instance Arbitrary Octree where
  arbitrary = sized genOctree

genOctree :: Int -> Gen Octree
genOctree 0 = Empty <$> arbitrary
genOctree n =
  let m = n `div` 2
      genSubtree = genOctree m
  in
    oneof [ Empty <$> arbitrary
          , Single <$> arbitrary <*> arbitrary
          , Node <$> arbitrary <*> genSubtree <*> genSubtree <*> genSubtree <*>
            genSubtree <*> genSubtree <*> genSubtree <*> genSubtree <*> genSubtree
          ]
    
  

propertyChecks :: TestTree
propertyChecks = testGroup "Property tests (QuickCheck)"
  [ QC.testProperty "updateAll of a singleton"
    $ \body -> updateAll 0 0.5 ([body] :: [Body]) == [body]
  , QC.testProperty "buildTree of a singleton"
    $ \body -> isSingle (buildTree [body])
  , QC.testProperty "buildTree of many bodies"
    $ \bodies -> (length bodies < 2) || isNode (buildTree bodies)
  ]
