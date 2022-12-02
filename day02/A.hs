allLines :: IO [String]
allLines = lines <$> getContents

trans :: Char -> Char
trans 'A' = 'X'
trans 'B' = 'Y'
trans 'C' = 'Z'

win :: Char -> Char
win 'A' = 'Y'
win 'B' = 'Z'
win 'C' = 'X'

pts :: Char -> Int
pts 'X' = 1
pts 'Y' = 2
pts 'Z' = 3

score :: Char -> Char -> Int
score a b
  | trans a == b = pts b + 3
  | win a == b   = pts b + 6
  | otherwise    = pts b

main :: IO ()
main = do
  ls <- allLines
  let gs = map (\(a:_:b:[]) -> (a, b)) ls
  print $ sum $ map (uncurry score) gs

