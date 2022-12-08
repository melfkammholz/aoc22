import Data.Char (ord)
import Data.List (transpose)

allLines :: IO [String]
allLines = lines <$> getContents

visibleRow :: [Int] -> [Bool] -> [Bool]
visibleRow = go (-1)
  where
    go l []     []      = []
    go l (x:xs) (s:ss)  = (s || x > l) : go (max l x) xs ss

visible :: [[Int]] -> [[Bool]] -> [[Bool]]
visible = zipWith visibleRow

top :: [[a]] -> [[a]]
top = transpose

right :: [[a]] -> [[a]]
right = map reverse

bottom :: [[a]] -> [[a]]
bottom = map reverse . top

topInv :: [[a]] -> [[a]]
topInv = transpose

rightInv :: [[a]] -> [[a]]
rightInv = map reverse

bottomInv :: [[a]] -> [[a]]
bottomInv = topInv . map reverse

main :: IO ()
main = do
  ls <- allLines
  let g       = map (\c -> ord c - ord '0') <$> ls
  let m0      = map (const False) <$> g
  let m1 = visible g m0
  let m2 = visible (top g) (top m1)
  let m3 = visible (right g) (right $ topInv m2)
  let m4 = visible (bottom g) (bottom $ rightInv m3)
  print $ sum . map (sum . (map fromEnum)) $ m4
