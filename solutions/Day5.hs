import           Common
import           Data.Foldable
import           Data.Map           (Map)
import qualified Data.Map.Strict    as M
import           Text.Parsec        hiding (Line)
import           Text.Parsec.String

main :: IO ()
main = run 5 solve1 solve2

data Point = Point
  { x :: Int
  , y :: Int
  } deriving (Eq, Show, Ord)

data Line = Line
  { from :: Point
  , to   :: Point
  } deriving (Eq, Show, Ord)

pLines :: Parser [Line]
pLines = many pLine
  where
    pLine = Line <$> (pPoint <* string " -> ") <*> (pPoint <* spaces)
    pPoint = Point <$> (pInt <* char ',') <*> pInt

(...) :: (Ord a, Enum a) => a -> a -> [a]
a ... b
  | a <= b = [a .. b]
  | otherwise = [a, pred a .. b]
infixl 7 ...

insertLine :: Bool -> Line -> Map Point Int -> Map Point Int
insertLine doDiag (Line (Point x1 y1) (Point x2 y2)) = M.unionWith (+) $ M.fromList ((,1) <$> points)
  where
    points :: [Point]
    points
      | x1 == x2 = Point x1 <$> y1 ... y2
      | y1 == y2 = flip Point y1 <$> x1 ... x2
      | not doDiag = []
      | otherwise = zipWith Point (x1 ... x2) (y1 ... y2)

overlapping :: Map k Int -> Int
overlapping = M.size . M.filter (>1)

withDiag :: Bool -> String -> String
withDiag diag = showE . fmap (overlapping . foldl' (flip $ insertLine diag) M.empty) . parse (pLines <* eof) "Day5.txt"

solve1 :: String -> String
solve1 = withDiag False

solve2 :: String -> String
solve2 = withDiag True
