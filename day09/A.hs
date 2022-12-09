import Control.Monad.State
import Data.Foldable (foldlM)
import qualified Data.Set as Set

type Rope = (Int, Int, Int, Int)

allLines :: IO [String]
allLines = lines <$> getContents

parseMotion :: String -> (Char, Int)
parseMotion (d:_:n) = (d, read n)

applyMotion :: Rope -> (Char, Int) -> State (Set.Set (Int, Int)) Rope
applyMotion p                (_, 0) = return p
applyMotion (hx, hy, tx, ty) (d, n) =
  do
    let (hx', hy') = move (hx, hy) d
    let (tx', ty') = moveTail (tx, ty) (hx', hy')
    modify $ Set.insert (tx', ty')
    applyMotion (hx', hy', tx', ty') (d, n - 1)
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
  let s = execState (foldlM applyMotion (0, 0, 0, 0) ms) Set.empty
  print $ Set.size s
