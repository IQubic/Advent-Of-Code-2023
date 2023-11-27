{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Common.Util ( module Common.Util
                   ) where
import Data.List
import Data.Maybe (mapMaybe)
import Data.Semigroup (Max(..), Min(..))
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty(..))
import Control.Lens
import Control.Comonad.Store
import Data.Map (Map)
import Data.Map qualified as M
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Set (Set)
import Data.Set qualified as S

-- | Loeb is just fancy memoization.
-- Each function in @f@ knows how to get a result,
-- provided it has the full structure
loeb :: Functor f => f (f a -> a) -> f a
loeb = moeb fmap

-- | Fancy function that does fancy things.
moeb :: (((a -> b) -> b) -> c -> a) -> c -> a
moeb f x = let go = f ($ go) x in go

-- | Composes all functions in a list from left to right.
composeAll :: Foldable f => f (a -> a) -> a -> a
composeAll = appEndo . getDual . foldMap (Dual . Endo)

-- | Checks if a given value is still in the list after running a filter.
elemIf :: (Foldable t, Eq a) => (a -> Bool) -> a -> t a -> Bool
elemIf p = elemOf (folded . filtered p)

-- | Counts the number of elements in a foldable that satisfy a given predicate.
countIf :: Foldable t => (a -> Bool) -> t a -> Int
countIf p = lengthOf (folded . filtered p)

-- | Computes the sum of all the elements that satisfy a given predicate.
sumIf :: (Foldable t, Num a) => (a -> Bool) -> t a -> a
sumIf = foldMapIf (coerced :: Iso' a (Sum a))
-- | Computes the product of all the elements that satisfy a given predicate.
productIf :: (Foldable t, Num a) => (a -> Bool) -> t a -> a
productIf = foldMapIf (coerced :: Iso' a (Product a))
-- | Computes the maximum of all the elements that satisfy a given predicate.
maximumIf :: (Foldable t, Ord a, Bounded a) => (a -> Bool) -> t a -> a
maximumIf = foldMapIf $ iso Max getMax
-- | Computes the minimum of all the elements that satisfy a given predicate.
minimumIf :: (Foldable t, Ord a, Bounded a) => (a -> Bool) -> t a -> a
minimumIf = foldMapIf $ iso Min getMin
-- | Mappends all elements that satisfy a predicate
foldIf :: (Foldable t, Monoid m) => (m -> Bool) -> t m -> m
foldIf = foldMapIf id

-- | Mappends all elements that satisfy a predicate
-- Uses an iso to create the monoid
foldMapIf :: (Foldable t, Monoid m)
           => Iso' a m
           -> (a -> Bool)
           -> t a
           -> a
foldMapIf _monoided p xs =
  _monoided # foldOf (folded . filtered p . _monoided) xs

-- | Gets the indices of all elements satisfying the predicate
indicesWhere :: Foldable f => (a -> Bool) -> f a -> [Int]
indicesWhere p xs = xs ^.. folded . filtered p . asIndex

-- | Converts a given sequence of digits from base @n@ to base @10@
fromBase :: (Num n, Foldable f) => n -> f n -> n
fromBase base = foldl' (\acc b -> base * acc + b) 0

-- | Converts a given number from base @10@ to base @n@
toBase :: Integral n => n -> n -> [n]
toBase base = reverse . unfoldr go
  where
    go n
      | n == 0 = Nothing -- Stop when previous division returned 0
      | otherwise = Just (mod, div) -- Keep the mod, use the div
      where
        (div, mod) = n `divMod` base

-- | Implementation of 'Data.List.nub' that uses 'Ord' for efficiency.
ordNub :: Ord a => [a] -> [a]
ordNub xs = foldr f (const []) xs S.empty
  where
    f x recur seen =
      case recur <$> S.alterF (, True) x seen of
        (True,  ys) -> ys
        (False, ys) -> x : ys

-- | Returns all the adjacent pairs of elements
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- | Returns overlapping chunks of length n
-- If there aren't at least n elements an empty list is returned.
slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n _
  | n <= 0 = []
slidingWindow _ []     = []
slidingWindow n (x:xs) =
  mapMaybe (gather n) $ toList $ duplicate (x :| xs)
     where
       -- gather tries to get the first n' elements
       -- fails if there aren't at least n' elements to get
       gather :: Int -> NonEmpty a -> Maybe [a]
       gather 0  _    = Just []
       gather n' (x' :| [])
         | n' == 1    = Just [x']
         | otherwise  = Nothing
       gather n' (x' :| (y:ys)) = gather (n'-1) (y:|ys) >>= (Just . (x':))

-- | Checks if all elements are equal.
same :: (Foldable t, Eq a) => t a -> Bool
same xs = all (head (toList xs) ==) xs

-- | Checks if all elements are different.
uniq :: (Foldable t, Ord a) => t a -> Bool
uniq xs = length xs == S.size (S.fromList $ toList xs)

-- | Finds all the configurations where each @k@ is given a different @a@
pickUnique :: forall k a. (Ord k, Ord a) => Map k (Set a) -> [Map k a]
pickUnique m = flip evalStateT S.empty $ do
    fmap M.fromList . for opts . traverse $ \poss -> do
      seen <- get
      pick <- lift $ S.toList (poss `S.difference` seen)
      pick <$ modify (S.insert pick)
  where
    -- Put the smallest sets first
    opts :: [(k, Set a)]
    opts = sortOn (S.size . snd) $ M.toList m

-- | Finds the first element where the function returns a @Just@ value
firstJust :: Foldable t
          => (a -> Maybe b)
          -> t a
          -> Maybe b
firstJust p = asum . map p . toList

-- | Finds the first fixed point of a given function.
fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f !x
  | x == x'   = x
  | otherwise = fixPoint f x'
  where x' = f x

-- | Finds the number of iterations until the
-- first fixed point of a given function.
fixPointLength :: Eq a => (a -> a) -> a -> Int
fixPointLength = go 1
  where
    go n f !x
      | x == x'   = n
      | otherwise = go (n+1) f x'
      where x' = f x

-- | Counts the number of times each element appears in a given foldable.
freqs :: (Foldable t, Ord a) => t a -> Map a Int
freqs = foldl' (\m val -> M.insertWith (+) val 1 m) M.empty

-- | Counts the number of times each Int appears in a given foldable.
intFreqs :: Foldable t => t Int-> IntMap Int
intFreqs = foldl' (\m val -> IM.insertWith (+) val 1 m) IM.empty

-- | Finds all the ways to select one element from a list
-- Returns the selected value and list of the other values
select :: [a] -> [(a,[a])]
select = go []
  where
    go _  [] = []
    go xs (y:ys) = (y, xs ++ ys) : go (y:xs) ys

-- | Gets all single item perterbations of the elements
perturbations
    :: Each s t a a
    => (a -> [a]) -- ^ Perturnbation function
    -> s          -- ^ Structure to perturb
    -> [t]        -- ^ Perturbations
perturbations = perturbationsBy each

-- | Gets all single item perterbations of the elements
-- Uses a given lens to get elements to change
perturbationsBy
    :: Conjoined p
    => Over p (Bazaar p a a) s t a a -- ^ Lens
    -> (a -> [a])                    -- ^ Perturnbation function
    -> s                             -- ^ Structure to perturb
    -> [t]                           -- ^ Perturbations
perturbationsBy p f = experiment f <=< holesOf p

-- | Iterates until a result of @Nothing@ is produced.
-- This is an inifitie loop if a @Just a@ result is never produced.
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f z = z : unfoldr (fmap dup . f) z
  where
    dup x = (x, x)

-- | Like @scanl@, but for Traverable structures
scanlT :: Traversable t => (b -> a -> b) -> b -> t a -> t b
scanlT f z = snd . mapAccumL (\x -> dup . f x) z
  where
    dup x = (x, x)

-- | Like @scanr@, but for Traverable structures
scanrT :: Traversable t => (a -> b -> b) -> b -> t a -> t b
scanrT f z = snd . mapAccumR (\x -> dup . flip f x) z
  where
    dup x = (x, x)

-- | Like @maximum@, but it checks for an empty structure
maximumMaybe :: (Ord a, Foldable t) => t a -> Maybe a
maximumMaybe xs
  | null xs   = Nothing
  | otherwise = Just $! maximum xs

-- | Like @minimum@, but it checks for an empty structure
minimumMaybe :: (Ord a, Foldable t) => t a -> Maybe a
minimumMaybe xs
  | null xs   = Nothing
  | otherwise = Just $! minimum xs

-- | Returns the last value for which a @Just@ result was given.
-- This is an infinite loop if no @Just@ is ever produced.
lastJust :: (a -> Maybe a) -> a -> a
lastJust f = go
  where
    go !x = case f x of
              Nothing -> x
              Just !y -> go y

-- | Finds the first element that appears at least twice.
firstDup :: Ord a => [a] -> Maybe a
firstDup = firstDupBy id

-- | Finds the first repeated element in the list
-- Uses a projection function to get only relevent info for comparison
firstDupBy :: Ord b => (a -> b) -> [a] -> Maybe a
firstDupBy f = go S.empty
  where
    go seen (x:xs)
      | f x `S.member` seen = Just x
      | otherwise           = go (f x `S.insert` seen) xs
    go _ [] = Nothing

-- | Strict Iterate
strictIterate :: (a -> a) -> a -> [a]
strictIterate f = go
  where
    go !x = x : go (f x)

-- | Strict (!!)
(!!!) :: [a] -> Int -> a
[]     !!! _ = error "Out of range"
(x:_)  !!! 0 = x
(x:xs) !!! n = x `seq` (xs !!! (n - 1))

-- | Apply a function @n@ times strictly.
times :: Int -> (a -> a) -> a -> a
times n f x
  | n <= 0    = x
  | otherwise = times (n-1) f $! f x

-- | Gives head, or a default value
headOr :: a -> [a] -> a
headOr def []    = def
headOr _   (x:_) = x
