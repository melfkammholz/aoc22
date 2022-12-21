import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.List (findIndex)
import qualified Data.Sequence as Seq

(%) :: Int -> Int -> Int
n % m = (n `mod` m + m) `mod` m

mix :: [Int] -> [Int]
mix xs = mix' (zip xs [0..]) (Seq.fromList $ zip xs [0..])
  where
    n = length xs

    mix' []          s = fst <$> toList s
    mix' ((x, i):xs) s = let j   = fromJust $ Seq.elemIndexL (x, i) s
                             k   = (j + x) % (n - 1)
                             s'  = Seq.deleteAt j s
                             s'' = Seq.insertAt k (x, i) s'
                         in mix' xs s''

sumCoords :: [Int] -> Int
sumCoords s = let k = fromJust $ findIndex (== 0) s
              in sum $ map (\i -> s !! ((i + k) % (length s))) [1000, 2000, 3000]

main :: IO ()
main = do
  xs <- (map read) . lines <$> getContents
  print $ sumCoords $ mix xs

