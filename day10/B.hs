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

crt :: [Int] -> String
crt = crt' 1
  where
    crt' _ []     = ""
    crt' i (x:xs) = if i `elem` [x, x + 1, x + 2]
                    then '#' : crt' ((i + 1) `mod` 40) xs
                    else '.' : crt' ((i + 1) `mod` 40) xs

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (l, r) = splitAt n xs
              in l : chunks n r

printCrt :: String -> IO ()
printCrt = mapM_ putStrLn . chunks 40

main :: IO ()
main = do
  ls <- allLines
  let ins = parseIns <$> ls
  let hist = reverse $ foldl runIns [1] ins
  printCrt (crt $ take 240 hist)

