import           Common
import           Data.IntMap        (IntMap)
import qualified Data.IntMap        as M
import           Text.Parsec
import           Text.Parsec.String

main :: IO ()
main = run "data/Day6.txt" solve1 solve2

pFish :: Parser [Int]
pFish = sepBy pInt (char ',')

solve1 :: String -> String
solve1 = solveWith 80

solve2 :: String -> String
solve2 = solveWith 256

solveWith :: Int -> String -> String
solveWith n = showE . fmap (sum . compose (replicate n step) . countFish) . parse (pFish <* eof) "Day6.txt"

countFish :: [Int] -> IntMap Int
countFish = M.unionWith (+) (M.fromList $ (,0) <$> [0..8]) . M.fromListWith (+) . map (,1)

step :: IntMap Int -> IntMap Int
step m = M.mapWithKey adjust m
  where
    at x = M.findWithDefault 0 x m
    adjust 6 _ = at 0 + at 7
    adjust 8 _ = at 0
    adjust k _ = at (k + 1)
