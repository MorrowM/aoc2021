{-# LANGUAGE RankNTypes #-}
module Common where
import           Control.Monad
import           Text.Parsec
import           Text.Parsec.String

readLines :: Read a => FilePath -> IO [a]
readLines = fmap (map read) . getLines

getLines :: FilePath -> IO [String]
getLines = fmap lines . readFile

pInt :: Parser Int
pInt = read <$> many1 digit

printE :: (Show a, Show b) => Either a b -> IO ()
printE = either print print

run :: FilePath -> (String -> IO a) -> (String -> IO b) -> IO ()
run fname solve1 solve2 = do
  input <- readFile fname
  solve1 input
  solve2 input
  pure ()
