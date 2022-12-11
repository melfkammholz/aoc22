import Data.List (isPrefixOf, splitAt)

data Instruction = Noop | AddX Int
  deriving (Show, Eq)

allLines :: IO [String]
allLines = lines <$> getContents

parseIns :: String -> Instruction
parseIns ins
  | ins == "noop"            = Noop
  | "addx " `isPrefixOf` ins = AddX (read $ drop (length "addx ") ins)
  | otherwise                = error "no instruction"

runIns :: [Int] -> Instruction -> [Int]
runIns xs@(x:_) Noop     = x : xs
runIns xs@(x:_) (AddX y) = (x + y) : x : xs

main :: IO ()
main = do
  ls <- allLines
  let ins = parseIns <$> ls
  let hist = reverse $ foldl runIns [1] ins
  let ss = [20,60..220]
  let xs = (\i -> hist !! (i - 1)) <$> ss
  print $ sum $ zipWith (*) xs ss

