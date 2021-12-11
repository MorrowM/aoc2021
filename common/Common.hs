{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Common
  ( module Text.Parsec
  , module Control.Monad
  , module Data.Maybe
  , module Data.List
  , module Data.Either
  , module Data.Bifunctor
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
  , module Data.Foldable
  , note
  , justify
  , readMaybe
  , middle
  ) where
import           Control.Exception
import           Control.Monad
import           Data.Bifunctor
import           Data.Either
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           GHC.Stack
import           Text.Parsec        hiding (Line, count, uncons)
import           Text.Parsec.String
import           Text.Printf
import           Text.Read

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

note :: a -> Maybe b -> Either a b
note a Nothing  = Left a
note _ (Just b) = Right b

justify :: String -> Maybe a -> a
justify = fromMaybe . error

middle :: HasCallStack => [a] -> a
middle xs = go xs xs
  where
    go (_:_:as) (_:bs) = go as bs
    go _ (x:_)         = x
    go _ _             = error "Common.middle: empty list"
