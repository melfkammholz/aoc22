import Data.List (nub)

solve :: Eq a => Int -> [a] -> Maybe Int
solve k l = solve' k (take (k - 1) l) (drop (k - 1) l)
  where
    solve' _ _ []    = Nothing
    solve' i r (c:b) =
      let r' = r ++ [c]
      in if length (nub r') == k
         then Just i
         else solve' (i + 1) (tail r') b

main :: IO ()
main = do
  l <- getLine
  let k = 4
  print $ solve k l

