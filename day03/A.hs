import Data.Char (isLower, ord)
import Data.List (intersect, nub, splitAt)

allLines :: IO [String]
allLines = lines <$> getContents

value :: Char -> Int
value c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 1 + 26

score :: String -> Int
score s = let (l, r) = splitAt (length s `div` 2) s
              int = nub $ intersect l r
          in sum $ map value int

main :: IO ()
main = do
  ls <- allLines
  print $ sum (map score ls)

