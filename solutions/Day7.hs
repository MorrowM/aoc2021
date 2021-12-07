import           Common

main :: IO ()
main = parseAndRun 7 (pInt `sepBy` pComma) (moveWith id) (moveWith $ \n -> n * (n + 1) `quot` 2)

moveWith :: (Int -> Int) -> [Int] -> Int
moveWith f locs = minimum [sum . map (\loc -> f . abs $ loc - goto) $ locs | goto <- [minimum locs .. maximum locs]]
