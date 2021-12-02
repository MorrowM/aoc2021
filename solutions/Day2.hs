import           Common
import           Data.Either
import           Data.Foldable
import           Text.Parsec
import           Text.Parsec.String

main :: IO ()
main = do
  input <- readFile "data/Day2.txt"
  printE $ solve1 input
  printE $ solve2 input

data Command
    = Forward Int
    | Up Int
    | Down Int

pCommands :: Parser [Command]
pCommands = many (pCommand <* newline) <* spaces
  where
    pCommand = choice
      [ pCom "forward" Forward
      , pCom "down" Down
      , pCom "up" Up
      ]

    pCom name constr = string name *> space *> (constr <$> pInt)

doIt :: ([Command] -> Int) -> String -> Either ParseError Int
doIt f input = f <$> parse (pCommands <* eof) "Day2.txt" input

solve1 :: String -> Either ParseError Int
solve1 = doIt (uncurry (*) . foldl' sumCommand1 (0,0))

sumCommand1 :: (Int, Int) -> Command -> (Int, Int)
sumCommand1 (x, y) com = case com of
  Forward n -> (x + n, y)
  Up n      -> (x, y - n)
  Down n    -> (x, y + n)

solve2 :: String -> Either ParseError Int
solve2 = doIt (prod3. foldl' sumCommand2 (0,0,0))
  where prod3 (x,y,_) = x * y

sumCommand2 :: (Int, Int, Int) -> Command -> (Int, Int, Int)
sumCommand2 (x, y, aim) com = case com of
  Forward n -> (x + n, y + aim * n, aim)
  Up n      -> (x, y, aim - n)
  Down n    -> (x, y, aim + n)
