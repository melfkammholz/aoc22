import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Grid a = Grid (Vector a) Int Int

type Pos = (Int, Int)

create :: Int -> Int -> a -> Grid a
create m n x = Grid (Vector.fromList $ replicate (m * n) x) m n

(!) :: Grid a -> Pos -> a
(Grid v _ n) ! (y, x) = v Vector.! (y * n + x)

toPos :: Int -> Int -> Int -> Pos
toPos m n z = (z `div` n, z `mod` n)

findIndex :: (a -> Bool) -> Grid a -> Maybe Pos
findIndex p (Grid v m n) = toPos m n <$> Vector.findIndex p v

adjust :: (a -> a) -> Pos -> Grid a -> Grid a
adjust f (y, x) (Grid v m n) = Grid (v Vector.// [(y * n + x, f $ v Vector.! (y * n + x))]) m n

bfs :: Grid Char -> Int
bfs g@(Grid v m n) = go g (Seq.singleton s) d ! e
  where
    d = adjust (const 0) s (create m n maxBound)

    go g q d
      | Seq.null q = d
      | otherwise  = let p   = fromJust $ Seq.lookup 0 q
                         q'  = Seq.deleteAt 0 q
                         ns  = next p d
                         pd  = (d ! p) + 1
                         d'  = foldr (adjust (const pd)) d ns
                         q'' = foldl (Seq.|>) q' ns
                     in go g q'' d'

    inBounds (y, x) = 0 <= y && y < m && 0 <= x && x < n

    incline p1 p2 = let f = if p1 == s then 'a' else g ! p1
                        t = if p2 == e then 'z' else g ! p2
                        d = fromEnum t - fromEnum f
                    in d <= 1

    shorter p1 d p2 = let d1 = d ! p1
                          d2 = d ! p2
                      in d1 + 1 < d2

    p &&& q = (&&) <$> p <*> q

    next (y, x) d = filter
                      (inBounds &&& incline (y, x) &&& shorter (y, x) d)
                      [(y, x + 1) , (y + 1, x), (y, x - 1), (y - 1, x)]

    s  = fromJust $ findIndex (== 'S') g
    e  = fromJust $ findIndex (== 'E') g

main :: IO ()
main = do
  ls <- lines <$> getContents
  let m = length ls
  let n = length $ head ls
  let g = Vector.fromList $ concat ls
  print $ bfs (Grid g m n)

