{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Common
  ( module Text.Parsec
  , Parser
  , readLines
  , getLines
  , parseAndRun
  , pInt
  , pComma
  , run
  , showE
  , todo
  , Todo (..)
  , compose
  ) where
import           Control.Exception
import           Control.Monad
import           Data.Foldable
import           Text.Parsec        hiding (Line, count)
import           Text.Parsec.String
import           Text.Printf

readLines :: Read a => FilePath -> IO [a]
readLines = fmap (map read) . getLines

getLines :: FilePath -> IO [String]
getLines = fmap lines . readFile

pInt :: Parser Int
pInt = read <$> many1 digit

pComma :: Parser Char
pComma = char ','

parseAndRun :: Show output => Int -> Parser input -> (input -> output) -> (input -> output) -> IO ()
parseAndRun day p solve1 solve2 = run day (runOne solve1) (runOne solve2)
  where runOne f = showE . fmap f . parse (p <* spaces <* eof) "<input>"

showE :: (Show a, Show b) => Either a b -> String
showE = either show show

run :: Int -> (String -> String) -> (String -> String) -> IO ()
run day solve1 solve2 = do
  printf "=== Day %d ===\n" day
  printf "Reading input...\n"
  input <- readFile $ printf "data/Day%d.txt" day
  printf "Part 1: %s\n" =<< completion (solve1 input)
  printf "Part 2: %s\n" =<< completion (solve2 input)
  pure ()
  where
    completion s = catch (evaluate s) (\(e :: Todo) -> pure "<todo>")

data Todo = Todo deriving (Show)
instance Exception Todo

todo :: a
todo = throw Todo

compose :: [a -> a] -> a -> a
compose = foldl' (.) id
