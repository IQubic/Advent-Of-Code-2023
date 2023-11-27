{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Common.Permutation
  ( Perm
  , runPerm
  , runPermOn
  , mkPerm
  , swap
  , rotateRight
  , rotateLeft
  , inverse
  , invert
  , isValid
  , size
  , backwards
  ) where

import Data.List
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup
import Data.Group
import Data.Function (fix)
import GHC.TypeNats (Nat, KnownNat, natVal)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V

type role Perm nominal
-- | A permuation of @n@ items
newtype Perm (n :: Nat) = P (Vector Int)
  deriving (Eq, Ord, Read, Show)

-- | Size of the list of elements permuted.
size :: Perm n -> Int
size (P v) = V.length v

-- | Validate a permutation. A valid permutation will map each element in the input
-- to a unique element in the output.
isValid :: forall n. KnownNat n => Perm n -> Bool
isValid (P vec) = V.and
                $ V.accumulate_ (\_ new -> new) (V.replicate n False) vec (V.replicate n True)
  where
    n = V.length vec

-- | Apply a permutation and get a list of ints
runPerm :: KnownNat n => Perm n -> [Int]
runPerm = runPermOn id

-- | Apply a permutation by using a projection function
runPermOn :: KnownNat n => (Int -> a) -> Perm n -> [a]
runPermOn f (P vec) = f <$> V.toList vec

-- | Helper function for making the size of a requested permutation available
-- while building the permutation.
withSize :: KnownNat n => (Int -> Perm n) -> Perm n
withSize f = fix (f . fromIntegral . natVal)

-- | Given a function mapping incoming indices to outgoing ones, construct
-- a new permutation value.
mkPerm :: KnownNat n => (Int -> Int) -> Perm n
mkPerm f = withSize $ \n ->
  P $ V.generate n $ \i -> f i `mod` n

-- | Permutation generated by swapping the elements at a pair of indices.
swap :: KnownNat n => Int -> Int -> Perm n
swap x y = withSize $ \n ->
  let x' = x `mod` n -- not evaluated when n == 0
      y' = y `mod` n
  in mkPerm $ \i ->
    if i == x' then y else if i == y' then x else i

-- | Permutation generated by rotating all the elements to the right.
rotateRight :: KnownNat n => Int -> Perm n
rotateRight = rotateLeft . negate

-- | Permutation generated by rotating all the elements to the left.
rotateLeft :: KnownNat n => Int -> Perm n
rotateLeft n = mkPerm $ \i -> i + n

-- | Permutation generated by reversing the order of the elements.
backwards :: KnownNat n => Perm n
backwards = mkPerm $ \i -> (-i) - 1

-- | Permutation generated by inverting another permutation.
inverse :: KnownNat n => Perm n -> Perm n
inverse (P vec) = P (V.accumulate_ (\_ new -> new) initial vec iota)
  where
    n       = V.length vec
    initial = V.replicate n (-1) -- -1 is arbitrary, should all be overwritten
    iota    = V.generate n id

-- | @a '<>' b@ is the permutation that first permutes with @a@ and
-- then with @b@.
instance KnownNat n => Semigroup (Perm n) where
  P x <> P y        = P (V.backpermute x y)
  sconcat (x :| xs) = foldl' (<>) x xs

instance KnownNat n => Monoid (Perm n) where
  mempty           = mkPerm fromIntegral
  mconcat []       = mempty
  mconcat (x : xs) = sconcat (x :| xs)

instance KnownNat n => Group (Perm n) where
  invert = inverse
