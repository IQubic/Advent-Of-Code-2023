{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Common.Box ( Box(..)
                  , Nat(..)
                  , size
                  , intersection
                  , intersections
                  , remove
                  , union
                  , unions
                  ) where

import GHC.List (foldl1')
import Control.Monad (guard, foldM)

import Data.Kind (Type)
import GHC.Stack.Types (HasCallStack)

-- | Natural numbers (used for type index)
data Nat = Z | S Nat

-- | An n-dimensional box.
data Box :: Nat -> Type where
  Pt  ::  Box Z -- ^ A single point
  Dim ::  !Int {- ^ inclusive lower bound -} ->
          !Int {- ^ exclusive upper bound -} ->
          Box n {- ^ lower dimensional box -} ->
          Box ('S n) -- ^ A box extended along an axis
deriving instance Show (Box n)
deriving instance Eq (Box n)
deriving instance Ord (Box n)

-- | Gets the size of a box
size :: Box n -> Int
size Pt              = 1
size (Dim lo hi box) = (hi - lo) * size box

-- | Intersection of two boxes
intersection :: Box n -> Box n -> Maybe (Box n)
intersection Pt Pt = Just Pt
intersection (Dim a b xs) (Dim c d ys) = do
  let x = max a c
  let y = min b d
  guard $ x < y
  zs <- intersection xs ys
  Just $ Dim x y zs

-- | Intersection of one or more boxes.
intersections :: HasCallStack => [Box n] -> Maybe (Box n)
intersections []     = error "intersectBoxes: empty intersection"
intersections (x:xs) = foldM intersection x xs

remove ::
  Box n {- ^ remove this -} ->
  Box n {- ^ from this -} ->
  [Box n] {- ^ leaving these -}
remove b1 b2 =
  case intersection b1 b2 of
    Nothing -> [b2]
    Just b  -> remove' b b2

-- | Worker for 'remove' where the first argument is a
-- subset of the second argument.
remove' :: Box n -> Box n -> [Box n]
remove' Pt Pt = []
remove' (Dim a b xs) (Dim c d ys) =
  [Dim c a ys | c < a] ++
  [Dim b d ys | b < d] ++
  [Dim a b zs | zs <- remove' xs ys]

-- | Compute the box that encompasses both arguments.
union :: Box n -> Box n -> Box n
union (Dim a b x) (Dim c d y) = Dim (min a c) (max b d) (x `union` y)
union Pt Pt = Pt

-- | Compute the box that encompasses all of the boxes in the list.
unions :: [Box n] -> Box n
unions = foldl1' union
