{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Common.Grid ( module Common.Grid
                   , module Linear.V2
                   , module Linear.V3
                   , module Data.Finite
                   , (*^)
                   , (^*)
                   ) where

import Common.Util (perturbations)
import Linear.V2
import Linear.V3
import Linear.Vector ((*^), (^*))
import Control.Lens
import Data.Map.Lens
import Data.Set.Lens
import Data.Tuple.Strict
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Monoid
import Data.Group
import Data.Finite
import GHC.TypeNats
import Data.List (transpose)
import Data.Ratio
import Data.Proxy
import Data.Containers.NonEmpty
import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Array.IArray (Array)
import Data.Array.IArray qualified as A

-- LATTICE POINTS
type Point = V2 Int
type FinPoint n = V2 (Finite n)
type Point3 = V3 Int
type FinPoint3 n = V3 (Finite n)

-- | Get X
getX :: R1 t => t a -> a
getX = view _x

-- | Get Y
getY :: R2 t => t a -> a
getY = view _y

-- | Get Z
getZ :: R3 t => t a -> a
getZ = view _z

-- | Gets the cardinal neighbors of a N-dimensional point.
cardinalNeighbors
  :: (Each (f a) (f a) a a, Num a)
  => f a
  -> [f a]
cardinalNeighbors = perturbations (\x -> [x+1,x-1])

-- | Gets the Chebyshev neighbors of an N-dimensional point.
fullNeighbors
  :: (Applicative f, Traversable f, Num (f a), Num a, Eq (f a))
  => f a
  -> [f a]
fullNeighbors pt =
    [ pt + dir
    | dir <- sequenceA (pure [-1,0,1])
    , dir /= pure 0
    ]

-- | Computes the Manhattan distance between two N-dimensional points.
manhattan :: (Foldable f, Num (f a), Num a) => f a -> f a -> a
manhattan pt1 pt2 = sum $ abs (pt1 - pt2)

-- | Computes the Cheybshev distance between two N-dimensional points.
chebyshev :: (Foldable f, Num (f a), Num a, Ord a) => f a -> f a -> a
chebyshev pt1 pt2 = maximum $ abs (pt1 - pt2)

-- CARDINAL DIRECTIONS
-- | Cardinal directions
data Dir = North | East | South | West
  deriving (Show, Eq, Ord, Enum)

-- | Enumeration of the directions
allDirs :: [Dir]
allDirs = [North ..]

-- | Gets the basis vector for a given direction
-- North is (0,1) here
dirPoint :: Num a => Dir -> V2 a
dirPoint North = V2   0   1
dirPoint East  = V2   1   0
dirPoint West  = V2 (-1)  0
dirPoint South = V2   0 (-1)

-- | Gets the basis vector for a given direction
-- North is (0,-1) here
dirPoint' :: Num a => Dir -> V2 a
dirPoint' North = V2   0 (-1)
dirPoint' East  = V2   1   0
dirPoint' West  = V2 (-1)  0
dirPoint' South = V2   0   1

-- | Rotates a point using a given direction
rotPoint :: Num a => Dir -> V2 a -> V2 a
rotPoint North (V2 x y) = V2   x    y
rotPoint East  (V2 x y) = V2   y  (-x)
rotPoint West  (V2 x y) = V2 (-y)   x
rotPoint South (V2 x y) = V2 (-x) (-y)

-- | Rotates a point using a given direction
rotFin :: forall n. KnownNat n => Dir -> FinPoint n -> FinPoint n
rotFin dir = over (mapping centeredFinite) (rotPoint dir)

-- | An Iso from a Finite N to a Rational
-- This is just the same number shifted so the center lies at the origin
centeredFinite :: forall n. KnownNat n => Iso' (Finite n) Rational
centeredFinite = iso (subtract r . (% 1) . getFinite)
                 (finite . numerator . (+ r))
  where
    r = fromIntegral (natVal (Proxy @n) - 1) % 2

-- | @<>@ performs a rotation.
-- @d1 <> d2@ rotates d1 in the d2 direction
instance Semigroup Dir where
  North <> dir   = dir
  dir   <> North = dir
  South <> dir   = invert dir
  dir   <> South = invert dir
  East  <> East  = South
  East  <> West  = North
  West  <> East  = North
  West  <> West  = South
  stimes n x = case n `mod` 4 of
    1 -> x
    2 -> x <> x
    3 -> x <> x <> x
    _ -> North
-- | North is the do nothing option.
instance Monoid Dir where
  mempty = North
-- | Invertng a @Dir@ rotates it 180 degrees.
instance Group Dir where
  invert North = South
  invert South = North
  invert East  = West
  invert West  = East
  pow = flip stimes
instance Abelian Dir

-- DIHEDRAL GROUP OF ORDER 8 (D4)
-- First rotate, then flip horizontally, if required
data D4 = D4 { d4Rot :: !Dir, d4Flip :: !Bool }
  deriving (Show, Eq, Ord)

-- | Enumeration of the D4 values
allD4 :: [D4]
allD4 = D4 <$> allDirs <*> [False, True]

-- This is left to right composition of D4s
instance Semigroup D4 where
  D4 x1 False <> D4 x2 y2 = D4 (x1 <> x2) y2
  D4 x1 True  <> D4 x2 y2 = D4 (x1 <> invert x2) (not y2)
instance Monoid D4 where
    mempty = D4 North False
instance Group D4 where
    invert (D4 x False) = D4 (invert x) False
    invert (D4 x True ) = D4 x          True

-- | Orients a point by a @D4@
orientPoint :: Num a => D4 -> V2 a -> V2 a
orientPoint (D4 North False) (V2 x y) = V2   x    y
orientPoint (D4 East  False) (V2 x y) = V2   y  (-x)
orientPoint (D4 West  False) (V2 x y) = V2 (-y)   x
orientPoint (D4 South False) (V2 x y) = V2 (-x) (-y)
orientPoint (D4 North True)  (V2 x y) = V2 (-x)   y
orientPoint (D4 East  True)  (V2 x y) = V2   y    x
orientPoint (D4 West  True)  (V2 x y) = V2 (-y) (-x)
orientPoint (D4 South True)  (V2 x y) = V2   x  (-y)

-- | Orients a point by a @D4@
orientFin :: KnownNat n => D4 -> FinPoint n -> FinPoint n
orientFin dir = over (mapping centeredFinite) (orientPoint dir)

-- 2D GRIDS
-- | Creates an array representing a 2D grid from a String
-- Requires newlines between the rows of the grid
asciiGridArray
  :: Num n
  => (Char -> a)
  -> String
  -> Array Point a
asciiGridArray f str = A.listArray (0, V2 maxX maxY)
                     $ concat $ transpose rows
  where
    maxX = length (head rows) - 1
    maxY = length rows - 1
    rows = map (map f) $ lines str

-- | Creates a map representing a 2D grid from a String
-- Requires newlines between the rows of the grid
asciiGridMap
  :: (Num n, Ord n)
  => (Char -> Maybe a)
  -> String
  -> Map (V2 n) a
asciiGridMap f = toMapOf (gridTraverse <. folding f) . lines

-- | Creates a set representing a 2D grid from a String
-- Requires newlines between the rows of the grid
asciiGridSet
  :: (Num n, Ord n)
  => (Char -> Bool)
  -> String
  -> Set (V2 n)
asciiGridSet f = setOf (gridTraverse . filtered f . asIndex) . lines

-- | An Indexed Traversal over the elements of a 2D structure
gridTraverse
  :: forall n a b f g. (Num n, Traversable f, Traversable g) =>
  IndexedTraversal (V2 n) (f (g a)) (f (g b)) a b
gridTraverse = icompose (flip toPoint) traversed traversed
  where
    toPoint :: Num n => Int -> Int -> V2 n
    toPoint x y = V2 (fromIntegral x) (fromIntegral y)

-- | Displays a Map of Points as a String
displayAsciiMap
    :: Char             -- ^ missing char
    -> Map Point Char   -- ^ tile map
    -> String
displayAsciiMap missing grid = unlines
    [ [ M.findWithDefault missing (V2 x y) grid
      | x <- [xMin .. xMax]]
    | y <- [yMin .. yMax]]
  where
    (V2 xMin yMin, V2 xMax yMax) = boundingBox $ M.keysSet grid

-- | Displays a Set of Points as a String
displayAsciiSet
    :: Char      -- ^ missing char
    -> Char      -- ^ present char
    -> Set Point -- ^ tile set
    -> String
displayAsciiSet missing here =
  displayAsciiMap missing . M.fromSet (const here)

-- | Returns @((V2 xMin yMin), (V2 xMax yMax))@.
boundingBox :: (Applicative f, Ord a) => Set (f a) -> (f a, f a)
boundingBox (IsNonEmpty g) = unpack $ foldMap1 pack g
  where
    pack p = T2 (Ap (Min <$> p)) (Ap (Max <$> p))
    unpack (T2 (Ap mn) (Ap mx)) = (getMin <$> mn, getMax <$> mx)
boundingBox _ = error "boundingBox: empty grid"

-- | Checks if a point is in a bounding box
inBoundingBox
    :: (Applicative f, Foldable f, Ord a)
    => (f a, f a)
    -> f a
    -> Bool
inBoundingBox (mn, mx) pt = and $ check <$> pt <*> mn <*> mx
  where
    check pt' min' max' = min' <= pt' && pt' <= max'

-- | Returns the minimum corner of a grid
-- | Only works on non-empty grids
minCorner :: Set (f a) -> f a
minCorner grid = case S.lookupMin grid of
                   Nothing  -> error "minCorner: empty grid"
                   Just min -> min

-- | Returns the maximum corner of a grid
-- | Only works on non-empty grids
maxCorner :: Set (f a) -> f a
maxCorner grid = case S.lookupMax grid of
                   Nothing  -> error "maxCorner: empty grid"
                   Just min -> min

-- | Shift corner to (0,0)
-- | Works with possibly empty sets
shiftToZero
    :: (Applicative f, Num (f a), Ord a)
    => Set (f a) -> Set (f a)
shiftToZero grid
  | S.null grid = S.mapMonotonic (subtract $ minCorner grid) grid
  | otherwise = grid

-- | Returns all the lattice points between the given endpoints
lineBetween :: Point -> Point -> [Point]
lineBetween pt0 pt1 = [pt0 + t *^ step | t <- [0 .. gcf]]
  where
    diff@(V2 dx dy) = pt1 - pt0
    gcf          = gcd dx dy
    step         = (`div` gcf) <$> diff

-- | Returns an infinite ray of points, including the starting point
-- North is (0,1) here
lineFrom :: Point -> Dir -> [Point]
lineFrom pt dir = iterate (+ dirPoint dir) pt

-- | Returns an infinite ray of points, including the starting point
-- North is (0,-1) here
lineFrom' :: Point -> Dir -> [Point]
lineFrom' pt dir = iterate (+ dirPoint' dir) pt

