import           Common
import           Data.Containers.ListUtils
import           Data.Ord
import           Math.Geometry.Grid
import           Math.Geometry.Grid.Square
import qualified Math.Geometry.GridMap      as G
import           Math.Geometry.GridMap.Lazy

main :: IO ()
main = parseAndRun 9 pBoard solve1 solve2

type Board = LGridMap RectSquareGrid Int
type Cell = Index RectSquareGrid

pBoard :: Parser Board
pBoard = convert <$> sepEndBy1 (many pDigit) newline
  where
    pDigit = read . pure <$> digit
    convert xss = lazyGridMap (rectSquareGrid r c) (concat xss)
      where
        r = length (head xss)
        c = length xss

solve1, solve2 :: Board -> Int
solve1 = sum . G.elems . G.map (+1) . lowPoints

isLowPoint :: Board -> Cell -> Int -> Bool
isLowPoint board k v = all (>v) (mapMaybe (`G.lookup` board) (neighbours (G.toGrid board) k))

lowPoints :: Board -> Board
lowPoints board = G.filterWithKey (isLowPoint board) board

expandBasin :: Board -> Cell -> [Cell]
expandBasin board k = (k:) . concatMap (expandBasin board) $ filter doExpand $ neighbours (G.toGrid board) k
  where
    doExpand nbr = justify ("error performing neighbor lookups, nbr = " <> show nbr <> ", k = " <> show k) $ do
      nbrVal <- G.lookup nbr board
      v <- G.lookup k board
      pure $ nbrVal > v && nbrVal < 9

basins :: Board -> [[Cell]]
basins board = nubOrd . expandBasin board <$> G.keys (lowPoints board)

solve2 = product . take 3 . sortBy (comparing Down) . map length . basins
