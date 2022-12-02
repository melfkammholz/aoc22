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

loose :: Char -> Char
loose 'A' = 'Z'
loose 'B' = 'X'
loose 'C' = 'Y'

pts :: Char -> Int
pts 'X' = 1
pts 'Y' = 2
pts 'Z' = 3

score :: Char -> Char -> Int
score a 'X' = pts (loose a)
score a 'Y' = pts (trans a) + 3
score a 'Z' = pts (win a) + 6

main :: IO ()
main = do
  ls <- allLines
  let gs = map (\(a:_:b:[]) -> (a, b)) ls
  print $ sum $ map (uncurry score) gs

