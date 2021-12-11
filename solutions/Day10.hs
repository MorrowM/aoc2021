import           Common
import           Text.Parsec.Error

main :: IO ()
main = parseAndRun 10 (lines <$> many anyChar) solve1 solve2

parseLine :: String -> Either ParseError ()
parseLine = parse (pLine <* eof) ""
  where
    pLine = void . many
      $  bet '(' ')'
      <|> bet '[' ']'
      <|> bet '{' '}'
      <|> bet '<' '>'
    bet open close = between (char open) (char close) pLine

solve1, solve2 :: [String] -> Int
solve1 = sum . map score
  where
    score (first errorMessages . parseLine -> Left (SysUnExpect s: _)) =
      case readMaybe s of
        Just ")" -> 3
        Just "]" -> 57
        Just "}" -> 1197
        Just ">" -> 25137
        _        -> 0
    score _ = 0

solve2 = middle . sort . filter (>0) . map (combineScores . scores)
  where
    combineScores = foldl' (\acc x -> acc * 5 + x) 0
    scores line = case expects =<< lefts [parseLine line] of
      ")" -> 1 : scores (line <> ")")
      "]" -> 2 : scores (line <> "]")
      "}" -> 3 : scores (line <> "}")
      ">" -> 4 : scores (line <> ">")
      _   -> []

    expects (errorMessages -> SysUnExpect "" : msgs)
      = [c | Expect s <- msgs, [c] <- maybeToList (readMaybe s), c `elem` ")]}>"]
    expects _ = []
