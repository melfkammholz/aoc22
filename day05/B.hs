import Data.Char (isDigit, isAlpha)
import Data.List (transpose)

allLines :: IO [String]
allLines = lines <$> getContents

parseStackLine :: String -> String
parseStackLine []        = []
parseStackLine [_]       = []
parseStackLine [_, _]    = []
parseStackLine (_:_:_:l:r)
  | isAlpha l = l : parseStackLine r
  | otherwise = ' ' : parseStackLine r

parseMove :: String -> (Int, Int, Int)
parseMove m = let r1         = drop (length "move ") m
                  (cnt, r2)  = span isDigit r1
                  r3         = drop (length " from ") r2
                  (from, r4) = span isDigit r3
                  r5         = drop (length " to ") r4
                  (to, _)    = span isDigit r5
              in (read cnt, read from - 1, read to - 1)

combineStackLines :: [String] -> [String]
combineStackLines ls = map trim <$> transpose $ parseStackLine <$> ("  " ++) <$> ls
  where
    trim = dropWhile (== ' ')

adjustAt :: Int -> (a -> a) -> [a] -> [a]
adjustAt _ _ []     = []
adjustAt 0 f (x:xs) = f x : xs
adjustAt i f (x:xs) = x : adjustAt (i - 1) f xs

move :: Int -> Int -> Int -> [String] -> [String]
move cnt from to sts = push $ pop sts
  where
    e    = take cnt (sts !! from)
    pop  = adjustAt from (drop cnt)
    push = adjustAt to (e ++)

main :: IO ()
main = do
  ls <- allLines
  let (stackLines, (_:_:moves)) = span (\(_:c:_) -> isAlpha c) ls
  let sts = combineStackLines stackLines
  let after = foldl (\sts (cnt, from, to) -> move cnt from to sts) sts (parseMove <$> moves)
  let code = foldr (\st c -> if null st then c else head st : c) "" after
  print code

