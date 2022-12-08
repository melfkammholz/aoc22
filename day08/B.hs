import Data.Char (ord)
import Data.List (transpose, zipWith4)

allLines :: IO [String]
allLines = lines <$> getContents

visibleRow :: [Int] -> [Int]
visibleRow = go
  where
    go []     = []
    go (h:hs) = count h hs : go hs

    count _ []     = 0
    count x (h:hs)
      | x > h     = 1 + count x hs
      | otherwise = 1

visible :: [[Int]] -> [[Int]]
visible = map visibleRow

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
  let g  = map (\c -> ord c - ord '0') <$> ls
  let m1 = visible g
  let m2 = topInv $ visible (top g)
  let m3 = rightInv $ visible (right g)
  let m4 = bottomInv $ visible (bottom g)
  let ss = zipWith4 (\w x y z -> w * x * y * z) (concat m1) (concat m2) (concat m3) (concat m4)
  print $ maximum ss
