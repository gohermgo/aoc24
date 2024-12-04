module Main where

import Data.Bifunctor
import Data.Functor
import Data.List

splitOnceOnChar     :: Char -> String -> Maybe (String, String)
splitOnceOnChar c s = elemIndex c s >>= (\i -> return (splitAt i s))

splitWhitespace     :: String -> Maybe (String, String)
splitWhitespace      = splitOnceOnChar ' '

splitOnChar :: Char -> String -> Maybe [String]
splitOnChar c s = splitOnceOnChar c s >>= uncurry rec
  where rec a b = splitOnChar c b <&> (a:)

readRow :: Read a => String -> Maybe (a, a)
readRow s = splitWhitespace s >>= \x -> return (readPair x)
  where readPair = bimap read read

splitChiefsListStrings :: [String] -> Maybe ([Int], [Int])
splitChiefsListStrings s = mapM readRow s >>= \x -> return (bimap sort sort (unzip x))

occurrenceInList :: Int -> [Int] -> Int
occurrenceInList val xs = length ( filter (==val) xs )

calculateChiefsListDistance :: [String] -> Maybe Int
calculateChiefsListDistance ls = splitChiefsListStrings ls >>= \(x, y) -> return (sum (zipWith absdiff x y))
  where absdiff x y = abs (x - y)


parseFilePart01 :: String -> Maybe Int
parseFilePart01 fileContent = calculateChiefsListDistance (lines fileContent)

calculateChiefsListAdjustedDistance :: [String] -> Maybe Int
calculateChiefsListAdjustedDistance ls = splitChiefsListStrings ls >>= \(x, y) -> return (sum (map (\val -> val * occurrenceInList val y) x) )

parseFilePart02 :: String -> Maybe Int
parseFilePart02 fileContent = calculateChiefsListAdjustedDistance (lines fileContent)
main :: IO ()
main = do
  s <- readFile "input.txt"
  print (parseFilePart01 s)
  print (parseFilePart02 s)
