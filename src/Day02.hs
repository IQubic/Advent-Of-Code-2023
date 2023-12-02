module Day02 where

import Common.Runner
import Common.Parser
import Data.Semigroup
import Data.Maybe (mapMaybe)
import Data.Bifunctor (second)
import Data.Map (Map)
import Data.Map qualified as M

part1 :: String -> Int
part1 = sum
      . mapMaybe (\(Game n xs) -> if validGame xs then Just n else Nothing)
      . pInput
  where
    validGame = all validPull
    validPull = all validColor
    validColor (R, n) = n <= 12
    validColor (G, n) = n <= 13
    validColor (B, n) = n <= 14

part2 :: String -> Int
part2 = sum
      . map (\(Game _ xs) -> power xs)
      . pInput
  where
    -- power uses a Map to store the max seen for each color
    power :: [Pull] -> Int
    power xs = let pulls = M.unionsWith (<>) (map pullSummary xs) in
      product $ fmap getMax pulls
    -- Use Max to compute the max
    pullSummary :: [Cube] -> Map Color (Max Int)
    pullSummary = M.fromList . map (second Max)

data Game  = Game Int [Pull]
type Pull  = [Cube]
type Cube  = (Color, Int)
data Color = R | G | B deriving (Eq, Ord, Show)

pInput :: String -> [Game]
pInput = pLines $ do
  id <- string "Game " *> pNumber <* string ": "
  Game id <$> pPull `sepBy1` string "; "
    where
      pPull = pCube `sepBy1` string ", "
      pCube = do
        n <- pNumber <* space
        c <- choice [ string "red"   $> R
                    , string "blue"  $> B
                    , string "green" $> G
                    ]
        pure (c, n)

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 2
