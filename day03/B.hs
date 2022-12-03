import Data.Char (isLower, ord)
import Data.List (intersect, nub, splitAt)

allLines :: IO [String]
allLines = lines <$> getContents

value :: Char -> Int
value c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 1 + 26

score :: [String] -> Int
score []         = 0
score (a:b:c:rs) = let (s:_) = a `intersect` b `intersect` c
                   in value s + score rs

main :: IO ()
main = do
  ls <- allLines
  print $ score ls

