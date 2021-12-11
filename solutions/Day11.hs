module Main (main, printBoard) where
import           Common
import           Common.Grid
import qualified Math.Geometry.GridMap as G
import           System.Console.ANSI

main :: IO ()
main = parseAndRun 11 pBoard solve1 solve2

pBoard :: Parser (Board Int)
pBoard = lazyGridMap (rectOctGrid 10 10) <$> many (pDigit <* spaces)

type Board = LGridMap RectOctGrid

solve1, solve2 :: Board Int -> Int
solve1 = sum . map (length . G.filter snd) . take 100 . startFlashing
solve2 = fst . justify "error: they never synchronised" . find (all snd . snd) . zip [0..] . startFlashing

startFlashing :: Board Int -> [Board (Int, Bool)]
startFlashing = iterate step . fmap (,False)

step :: Board (Int, Bool) -> Board (Int, Bool)
step board = doFlash (bimap (+1) (const False) <$> board)
  where
    increment = foldl' (flip $ G.adjust (first (+1)))
    toFlash = G.keys . G.filter (\(v, flashed) -> v >= 10 && not flashed)
    doFlash b = case toFlash b of
      [] -> fmap (\(x, flashed) -> (if flashed then 0 else x, flashed)) b
      ks -> doFlash $ foldl' (\b' k -> G.adjust (second (const True)) k $ increment b' $ neighbours (G.toGrid b) k) b ks

-- | Debug function for printing boards with fancy colors
printBoard :: Board (Int, Bool) -> IO ()
printBoard
  = traverse_ (\(x, b) -> if b then color Green x else putStr x)
  . (<>[("\n", False)]) . intercalate [("\n", False)] . chunksOf 10 . map (first show) . G.elems
