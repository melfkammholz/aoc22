import Data.Char (isDigit)

allLines :: IO [String]
allLines = lines <$> getContents

parse :: String -> (Int, Int, Int, Int)
parse l = let (a,_:r1) = span isDigit l
              (b,_:r2) = span isDigit r1
              (c,_:r3) = span isDigit r2
              (d,_)    = span isDigit r3
          in (read a, read b, read c, read d)

somewhatContained :: Int -> Int -> Int -> Int -> Bool
somewhatContained a b c d =  a <= c && c <= b
                          || a <= d && d <= b
                          || c <= a && a <= d
                          || c <= b && b <= d

main :: IO ()
main = do
  ls <- allLines
  let ps = map parse ls
  print $ sum $ (fromEnum . \(a, b, c, d) -> somewhatContained a b c d) <$> ps

