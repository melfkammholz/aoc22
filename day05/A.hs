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
              in (read cnt, read from, read to)

combineStackLines ls = map (dropWhile (== ' ')) <$> transpose $ parseStackLine <$> ("  " ++) <$> ls

move :: Int -> Int -> Int -> [String] -> [String]
move 0   _    _  sts = sts
move cnt from to sts = move (cnt - 1) from to (push $ pop sts)
  where
    e        = head (sts !! (from - 1))
    pop  sts = map (\(n, st) -> if n == from then drop 1 st else st) (zip [1..] sts)
    push sts = map (\(n, st) -> if n == to then e : st else st) (zip [1..] sts)

main :: IO ()
main = do
  ls <- allLines
  let (stackLines, (_:_:moves)) = span (\(_:c:_) -> isAlpha c) ls
  let sts = combineStackLines stackLines
  let after = foldl (\sts (cnt, from, to) -> move cnt from to sts) sts (parseMove <$> moves)
  let code = foldr (\c st -> head c : st) "" after
  print code

