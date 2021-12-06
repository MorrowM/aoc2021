import           Common

main :: IO ()
main = run 1 solve1 solve2

withFunction :: ([Int] -> Int) -> String -> String
withFunction f = show . f . map read . lines

solve1 :: String -> String
solve1 = withFunction count

count :: [Int] -> Int
count = length . filter id . (zipWith (<) <*> tail)


solve2 :: String -> String
solve2 = withFunction $ count . slidingWindow
  where
    slidingWindow xs = zipWith3 (\a b c -> a + b + c) xs (drop 1 xs) (drop 2 xs)

