{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Common.Search where

import Data.Foldable (foldl')
import Data.Set (Set)
import Data.Set qualified as S
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.OrdPSQ (OrdPSQ)
import Data.OrdPSQ qualified as PQ

-- | DFS from a single starting state
dfs :: Ord a
    => (a -> [a]) -- ^ Generation of Next States
    ->  a         -- ^ Initial State
    -> [a]        -- ^ Reachable States
dfs = dfsOn id

-- | DFS from a single starting state
-- | Uses a projection to determine if two states are equal
dfsOn :: Ord b
      => (a -> b)   -- ^ State Repersentation
      -> (a -> [a]) -- ^ Generation of Next States
      -> a          -- ^ Initial State
      -> [a]        -- ^ Reachable States
dfsOn rep nexts start = loop S.empty [start]
  where
    loop !seen = \case
      [] -> []
      x:xs
        | S.member r seen ->     loop seen xs
        | otherwise       -> x : loop seen' (nexts x ++ xs)
        where
          r     = rep x
          seen' = S.insert r seen


-- | BFS from a single starting state
bfs :: Ord a
    => (a -> [a]) -- ^ Generation of Next States
    ->  a         -- ^ Initial State
    -> [a]        -- ^ Reachable States
bfs nexts start = bfsOnN id nexts [start]

-- | BFS from mulitple starting states
bfsN :: Ord a
    => (a -> [a]) -- ^ Generation of Next States
    -> [a]        -- ^ Initial States
    -> [a]        -- ^ Reachable States
bfsN = bfsOnN id

-- | BFS from a single starting state
-- | Uses a projection to determine if two states are equal
bfsOn :: Ord b
      => (a -> b)   -- ^ State Repersentation
      -> (a -> [a]) -- ^ Generation of Next States
      -> a          -- ^ Initial State
      -> [a]        -- ^ Reachable States
bfsOn rep nexts start = bfsOnN rep nexts [start]

-- | BFS from multiple starting states
-- | Uses a projection to determine if two states are equal
bfsOnN :: Ord b
       => (a -> b)   -- ^ State Repersentation
       -> (a -> [a]) -- ^ Generation of Next States
       -> [a]        -- ^ Initial States
       -> [a]        -- ^ Reachable States
bfsOnN rep nexts start = go S.empty (Seq.fromList start)
  where
    go !seen = \case
      Seq.Empty -> []
      (x Seq.:<| work)
        | r `S.member` seen -> go seen work
        | otherwise         -> x : go seen' work'
        where
          r     = rep x
          seen' = S.insert r seen
          work' = foldl' (Seq.|>) work (nexts x)

-- | A* search. For best results, the heuristic must
-- underestimate the cost to the goal
aStar :: Ord a
      => (a -> [(a, Int)]) -- ^ Generations of Next States
      -> (a -> Int)        -- ^ Huristic
      -> a                 -- ^ Initial state
      -> [(a, Int)]        -- ^ Reachable States
aStar = aStarOn id

aStarOn :: (Ord a, Ord b)
        => (a -> b)          -- ^ State Repersentation
        -> (a -> [(a, Int)]) -- ^ Generations of Next States
        -> (a -> Int)        -- ^ Huristic
        -> a                 -- ^ Initial state
        -> [(a, Int)]        -- ^ Reachable States
aStarOn rep nexts heuristic start =
  go S.empty (PQ.singleton start 0 0)
  where
    go !seen !q = case PQ.minView q of
      Nothing -> []
      Just (x, _, cost, work)
        | r `S.member` seen -> go seen work
        | otherwise         -> (x, cost) : go seen' work'
        where
          r     = rep x
          seen' = S.insert r seen
          work' = foldl' addWork work (nexts x)
          -- Only overwrite unvisited nodes if they have larger costs
          addWork w (x', stepCost) =
            case PQ.lookup x' w of
              Nothing -> PQ.insert x' (cost' + heuristic x') cost' w
              Just (_, oldCost) ->
                if cost' < oldCost
                then PQ.insert x' (cost' + heuristic x') cost' w
                else w
            where
              cost' = cost + stepCost
