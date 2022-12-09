import Control.Monad.State
import Data.Foldable (foldlM)
import Data.List (replicate)
import qualified Data.Set as Set

type Rope = [(Int, Int)]

allLines :: IO [String]
allLines = lines <$> getContents

parseMotion :: String -> (Char, Int)
parseMotion (d:_:n) = (d, read n)

applyMotion :: Rope -> (Char, Int) -> State (Set.Set (Int, Int)) Rope
applyMotion ps (_, 0) = return ps
applyMotion ps (d, n) =
  do
    let (ps', (tx, ty)) = foldl (\(qs, h) n -> (h : qs, moveTail n h)) ([], move (head ps) d) (tail ps)
    modify $ Set.insert (tx, ty)
    applyMotion (reverse ((tx, ty):ps')) (d, n - 1)
  where
    move (x, y) 'R' = (x + 1, y)
    move (x, y) 'L' = (x - 1, y)
    move (x, y) 'U' = (x, y + 1)
    move (x, y) 'D' = (x, y - 1)

    moveTail (tx, ty) (hx, hy)
      | abs (ty - hy) <= 1 && abs (tx - hx) <= 1 = (tx, ty)
      | abs (ty - hy) == 2 && abs (tx - hx) == 1 = (hx, ty - signum (ty - hy))
      | abs (ty - hy) == 1 && abs (tx - hx) == 2 = (tx - signum (tx - hx), hy)
      | (tx', ty') /= (hx, hy)                   = (tx', ty')
      where
        (tx', ty') = (tx + signum (hx - tx), ty + signum (hy - ty))

main :: IO ()
main = do
  ls <- allLines
  let ms = parseMotion <$> ls
  let rope = replicate 10 (0, 0)
  let s = execState (foldlM applyMotion rope ms) Set.empty
  print $ Set.size s
