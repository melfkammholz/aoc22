import Data.List (unfoldr)

type SNAFU = String

toDec :: SNAFU -> Int
toDec = foldl (\r d -> r * 5 + di d) 0
  where
    di '=' = -2
    di '-' = -1
    di '0' = 0
    di '1' = 1
    di '2' = 2

toSNAFU :: Int -> SNAFU
toSNAFU = reverse . unfoldr f
  where
    s = ['0', '1', '2', '=', '-']

    f 0 = Nothing
    f x = let d = x `mod` 5
          in Just (s !! d, (if d > 2 then 5 else 0 + x) `div` 5)

main :: IO ()
main = do
  ls <- lines <$> getContents
  print $ toSNAFU (sum $ map toDec ls)

