import Data.List (nub)

solve :: Int -> Int -> String -> String -> Maybe Int
solve _ _ _ []    = Nothing
solve i k r (c:b) =
  let r' = r ++ [c]
  in if length (nub r') == k
     then Just i
     else solve (i + 1) k (tail r') b

main :: IO ()
main = do
  l <- getLine
  let k = 4
  print $ solve k k (take (k - 1) l) (drop (k - 1) l)

