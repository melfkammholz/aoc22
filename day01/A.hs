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
splitOn y xs = let (l, r) = span (/= y) xs
               in if null r then [l]
                  else l : splitOn y (tail r)

main :: IO ()
main = do
  ls <- allLines
  let gs = map (map read) (splitOn "" ls)
  print $ maximum (map sum gs)

