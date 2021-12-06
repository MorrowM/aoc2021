import           Common
import           Data.List
import           Debug.Trace
import           Text.Parsec
import           Text.Parsec.String

main :: IO ()
main = run "data/Day4.txt" solve1 solve2

solve1 :: String -> String
solve1 = withInput runBingo

solve2 :: String -> String
solve2 = withInput runBingo'

withInput :: ([Int] -> [Board] -> (Int, Board)) -> String -> String
withInput bingoFunc = showE . fmap (uncurry score . uncurry bingoFunc . (\x -> traceShow (length $ snd x) x)) . parse (pNumsAndBoards <* eof) "Day4.txt"

data Mark = Unmarked | Marked deriving (Eq, Show, Ord)
data Cell = Cell { cell :: Int, status :: Mark } deriving (Eq, Show, Ord)
newtype Board = Board [[Cell]] deriving Show

mark :: Cell -> Cell
mark c = c { status = Marked }

marked :: Cell -> Bool
marked (Cell _ Marked) = True
marked _               = False

markBoard :: Int -> Board -> Board
markBoard n (Board rows) = Board $ (fmap . fmap) (\c -> if cell c == n then mark c else c) rows

isWin :: Board -> Bool
isWin (Board rows) = any (all marked) rows || any (all marked) (transpose rows)

runBingo :: [Int] -> [Board] -> (Int, Board)
runBingo [] _ = error "no boards won!"
runBingo (n:ns) boards =
  let boards' = markBoard n <$> boards
  in case find isWin boards' of
    Nothing    -> runBingo ns boards'
    Just board -> (n, board)

runBingo' :: [Int] -> [Board] -> (Int, Board)
runBingo' nums = last . go nums
  where
    go [] _ = []
    go (n:ns) boards =
      let boards' = markBoard n <$> boards
          (winners, losers) = partition isWin boards'
      in map (n,) winners ++ go ns losers

score :: Int -> Board -> Int
score n (Board rows) = (*n) . sum . map cell . filter ((==Unmarked) . status) $ concat rows

pNumsAndBoards :: Parser ([Int], [Board])
pNumsAndBoards = (,) <$> (pNums <* spaces) <*> pBoards
  where
    pNums = sepBy pInt (char ',')
    pBoards = sepBy1 pBoard newline
    pBoard = Board <$> pCell `sepBy1` char ' ' `endBy1` newline
    pCell = many (char ' ') *> fmap (`Cell` Unmarked) pInt
