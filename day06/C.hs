import Data.List (splitAt)
import qualified Data.Map as M
import qualified Data.Sequence as S

solve :: Int -> String -> Maybe Int
solve k s = solve' k (S.fromList $ l) (foldl insert M.empty l) r
  where
    (l, r) = splitAt (k - 1) s

    test m = Just k == M.foldrWithKey (\k c mv -> if c <= 1 then pure (+ c) <*> mv else Nothing) (Just 0) m

    insert m c = M.insertWith (+) c 1 m

    delete m c = M.adjust (subtract 1) c m

    solve' _ _ _ []    = Nothing
    solve' i q m (c:b) =
      let m' = insert m c
      in if test m'
         then Just i
         else let m'' = delete m' (S.index q 0)
                  q'  = S.deleteAt 0 q S.|> c
              in solve' (i + 1) q' m'' b

main :: IO ()
main = do
  l <- getLine
  let k = 1004
  print $ solve k l

