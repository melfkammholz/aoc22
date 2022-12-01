import Data.List (span, sort, partition)
import System.IO (isEOF)

allLines :: IO [String]
allLines = lines <$> getContents

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn y xs
  | null r    = if null l then [] else [l]
  | otherwise = l : splitOn y (tail r)
  where
    (l, r) = span (/= y) (dropWhile (== y) xs)

mom [x] = x
mom xs  = mom . medians $ xs
  where
    median xs = sort xs !! (length xs `div` 2)
    medians [] = []
    medians xs = let (l, r) = splitAt 5 xs
                 in median l : medians r

quickselect xs 0 = ([], xs)
quickselect xs i
  | i < n     = let (l, r) = quickselect ys i
                in (l, r ++ zs)
  | otherwise = let (l, r) = quickselect zs (i - n)
                in (ys ++ l, r)
  where
    p = mom xs
    (ys, zs) = partition (< p) xs
    n = length ys

main :: IO ()
main = do
  ls <- allLines
  let gs = map (map read) (splitOn "" ls)
  let ss = map sum gs
  let (_, r) = quickselect ss (length ss - 3)
  print $ sum r
  -- print $ sum (take 3 $ reverse $ sort $ map sum gs)

