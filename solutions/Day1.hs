import           Common

main :: IO ()
main = run "data/Day1.txt" solve1 solve2

withFunction :: ([Int] -> Int) -> String -> IO ()
withFunction f = print . f . map read . lines

solve1 :: String -> IO ()
solve1 = withFunction count

count :: [Int] -> Int
count = length . filter id . (zipWith (<) <*> tail)


solve2 :: String -> IO ()
solve2 = withFunction $ count . slidingWindow
  where
    slidingWindow xs = zipWith3 (\a b c -> a + b + c) xs (drop 1 xs) (drop 2 xs)

