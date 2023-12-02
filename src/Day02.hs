module Day02 where

import Common.Runner
import Common.Parser
import Common.Util
import Control.Applicative (asum)
import Data.Semigroup
import Data.Maybe (mapMaybe)

part1 :: String -> Int
part1 = sum
      . mapMaybe (\(Game n xs) -> if validGame xs then Just n else Nothing )
      . pInput
  where
    validGame = all validPull
    validPull = all validColor
    validColor (R n) = n <= 12
    validColor (G n) = n <= 13
    validColor (B n) = n <= 14

part2 :: String -> Int
part2 = sum
      . map (\(Game _ xs) -> power xs)
      . pInput
  where
    -- faldMap gets the max for each color across each pull
    power xs = let (Max r,Max g,Max b) = foldMap pullSummary xs in
      r*g*b
    -- Aggregate the pull colors. Because I don't sort things,
    -- I need to do this jank
    pullSummary = foldr toMonoid mempty
    toMonoid (R n) (_,g,b) = (Max n, g,     b    )
    toMonoid (G n) (r,_,b) = (r,     Max n, b    )
    toMonoid (B n) (r,g,_) = (r,     g,     Max n)

data Game = Game Int [Pull]
type Pull = [Cube]
data Cube = R Int | G Int | B Int

pInput :: String -> [Game]
pInput = pLines $ do
  id <- string "Game " *> pNumber <* string ": "
  Game id <$> pPull `sepBy1` string "; "
    where
      pPull = pCube `sepBy1` string ", "
      pCube = do
        n <- pNumber <* space
        f <- asum [ string "red"   $> R
                  , string "blue"  $> B
                  , string "green" $> G
                  ]
        pure $ f n

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 2
