import Data.List (span)
import System.IO (isEOF)

allLines :: IO [String]
allLines = do 
  done <- isEOF
  if not done then do
    s <- getLine
    ss <- allLines
    return (s:ss)
  else return []

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn y xs
  | null r    = if null l then [] else [l]
  | otherwise = l : splitOn y (tail r)
  where
    (l, r) = span (/= y) (dropWhile (== y) xs)

main :: IO ()
main = do
  ls <- allLines
  let gs = map (map read) (splitOn "" ls)
  print $ maximum (map sum gs)

