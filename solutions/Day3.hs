{-# LANGUAGE RankNTypes #-}
import           Common

main :: IO ()
main = run 3 solve1 solve2

withFunction :: ([String] -> Int) -> String -> String
withFunction f = show . f . lines

mostCommon :: (forall a. Ord a => a -> a -> a) -> [Char] -> Int
mostCommon fun str = snd $ fun (cnt '1', 1) (cnt '0', 0)
  where
    cnt c = length $ filter (==c) str

fromBin :: [Int] -> Int
fromBin = foldr (\dig acc -> dig + 2 * acc) 0 . reverse

solve1 :: String -> String
solve1 = withFunction go
  where
    go strs = gamma * epsilon
      where
        bits = transpose strs
        gamma = fromBin $ mostCommon max <$> bits
        epsilon = fromBin $ mostCommon min <$> bits

solve2 :: String -> String
solve2 = todo
