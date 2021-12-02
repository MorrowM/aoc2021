import           Common

main :: IO ()
main = do
  input <- readLines "data/Day1.txt"
  print $ solve1 input
  print $ solve2 input

solve1 :: [Int] -> Int
solve1 = length . filter id . (zipWith (<) <*> tail)

slidingWindow :: [Int] -> [Int]
slidingWindow xs = zipWith3 (\a b c -> a + b + c) xs (drop 1 xs) (drop 2 xs)

solve2 :: [Int] -> Int
solve2 = solve1 . slidingWindow
