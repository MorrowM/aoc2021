import           Common
import           Data.Foldable
import           Text.Parsec
import           Text.Parsec.String

fname :: String
fname =  "data/Day2.txt"

main :: IO ()
main = run 2 solve1 solve2

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

withFunction :: ([Command] -> Int) -> String -> String
withFunction f = showE . fmap f . parse (pCommands <* eof) fname

solve1 :: String -> String
solve1 = withFunction $ uncurry (*) . foldl' go (0,0)
  where
    go (x, y) com = case com of
      Forward n -> (x + n, y)
      Up n      -> (x, y - n)
      Down n    -> (x, y + n)

solve2 :: String -> String
solve2 = withFunction $ prod3 . foldl' go (0,0,0)
  where
    prod3 (x,y,_) = x * y

    go (x, y, aim) com = case com of
      Forward n -> (x + n, y + aim * n, aim)
      Up n      -> (x, y, aim - n)
      Down n    -> (x, y, aim + n)
