{-# LANGUAGE RankNTypes #-}
module Common where
import           Control.Monad
import           Data.Foldable
import           Text.Parsec
import           Text.Parsec.String

readLines :: Read a => FilePath -> IO [a]
readLines = fmap (map read) . getLines

getLines :: FilePath -> IO [String]
getLines = fmap lines . readFile

pInt :: Parser Int
pInt = read <$> many1 digit

showE :: (Show a, Show b) => Either a b -> String
showE = either show show

run :: FilePath -> (String -> String) -> (String -> String) -> IO ()
run fname solve1 solve2 = do
  input <- readFile fname
  putStrLn $ solve1 input
  putStrLn $ solve2 input
  pure ()

compose :: [a -> a] -> a -> a
compose = foldl' (.) id
