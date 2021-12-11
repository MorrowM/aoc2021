import           Common
import qualified Data.Map as M

main :: IO ()
main = parseAndRun 8 pInput solve1 solve2

pInput :: Parser [([[Segment]], [[Segment]])]
pInput = sepBy1 pLine newline
  where
    pLine = (,) <$> (endBy (many1 pSegment) (char ' ') <* string "| ") <*> sepBy (many1 pSegment) (char ' ')
    pSegment = choice
      [ pSeg A 'a', pSeg B 'b', pSeg C 'c', pSeg D 'd', pSeg E 'e', pSeg F 'f', pSeg G 'g'
      ]
    pSeg constr c = constr <$ char c :: Parser Segment

data Segment = A | B | C | D | E | F | G
  deriving (Show, Eq, Ord, Enum, Bounded)

solve1, solve2 :: [([[Segment]], [[Segment]])] -> Int
solve1 = sum . map (length . filter ((`elem` [2,3,4,7]) . length) . snd)

solve2 input = sum
  [ digitsToInt $ ifInvalidSegs . segsToDigit . sort . map mapping <$> digits
  | (segs, digits) <- input
  -- Error messages ---
  , let ifNotUniqueMapping = justify $ "Couldn't find a unique mapping:\nsegments = " <> show segs <> "\ndigits = " <> show digits
  , let ifInvalidSegs = justify $ "Invalid seqment sequence:\nsegments = " <> show segs <> "\ndigits = " <> show digits
  ---------------------
  , let mapping = ifNotUniqueMapping . unique $ foldl' eliminate allMappings segs
  ]

allMappings :: [Segment -> Segment]
allMappings = map ((M.!) . M.fromList . zip [A ..]) $ permutations [A ..]

eliminate :: [Segment -> Segment] -> [Segment] -> [Segment -> Segment]
eliminate mappings segments = filter consistent mappings
  where
    consistent f = case length segments of
      2 -> sort (f <$> segments) == [C, F]
      3 -> sort (f <$> segments) == [A, C, F]
      4 -> sort (f <$> segments) == [B, C, D, F]
      5 -> sort (f <$> segments) `elem` [[A, B, D, F, G], [A, C, D, E, G], [A, C, D, F, G]]
      _      -> True

segsToDigit :: [Segment] -> Maybe Int
segsToDigit [A, B, C, E, F, G]    = Just 0
segsToDigit [C, F]                = Just 1
segsToDigit [A, C, D, E, G]       = Just 2
segsToDigit [A, C, D, F, G]       = Just 3
segsToDigit [B, C, D, F]          = Just 4
segsToDigit [A, B, D, F, G]       = Just 5
segsToDigit [A, B, D, E, F, G]    = Just 6
segsToDigit [A, C, F]             = Just 7
segsToDigit [A, B, C, D, E, F, G] = Just 8
segsToDigit [A, B, C, D, F, G]    = Just 9
segsToDigit _                     = Nothing

digitsToInt :: [Int] -> Int
digitsToInt = read . concatMap show

unique :: [a] -> Maybe a
unique [x] = Just x
unique _   = Nothing
