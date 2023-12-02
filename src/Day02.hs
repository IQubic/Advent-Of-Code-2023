module Day02 where

import Common.Runner
import Common.Parser
import Linear.V3
import Data.Functor ((<&>))
import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe, mapMaybe)

part1 :: String -> Int
part1 = sum
      . mapMaybe (\(n, xs) -> if all validPull xs then Just n else Nothing)
      . pInput
  where
    -- Run a pairwise comparison
    validPull = and . liftA2 (>=) (V3 12 13 14)

part2 :: String -> Int
part2 = sum
      . map (\(_, xs) -> power xs)
      . pInput
  where
    -- For each color, get the largest quantity ever pulled
    power :: [Pull] -> Int
    power = product . foldr (liftA2 max) 0

-- Store in RGB order, with 0 as default
type Game  = (Int, [Pull])
type Pull  = V3 Int
data Color = R | G | B deriving (Eq, Ord, Show)

pInput :: String -> [Game]
pInput = pLines $ do
  id <- string "Game " *> pNumber <* string ": "
  pulls <- pPull `sepBy1` string "; "
  pure (id, pulls)
    where
      pPull = toV3 <$> pCube `sepBy1` string ", "
      pCube = do
        n <- pNumber <* space
        c <- choice [ string "red"   $> R
                    , string "blue"  $> B
                    , string "green" $> G
                    ]
        pure (c, n)
      -- Store data in RGB order with 0 as default
      toV3 :: [(Color, Int)] -> V3 Int
      toV3 cs = V3 R G B <&> \c -> fromMaybe 0 (lookup c cs)

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 2
