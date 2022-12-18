import Control.Applicative ((<|>))
import Control.Exception (assert)
import Data.List (intercalate, transpose)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec (Parsec, parse, many)
import Text.Parsec.Char (char)

trace :: String -> a -> a
trace s e = assert (unsafePerformIO $ putStrLn s >> return True) e

trace' :: Show a => a -> a
trace' e = trace (show e) e

type Parser = Parsec String ()

data Shape = Minus | Plus | Angle | Pipe | Square
  deriving (Show, Eq, Enum, Bounded)

footprint :: Shape -> Vector (Vector Char)
footprint Minus  = Vector.fromList [Vector.fromList "####"]
footprint Plus   = Vector.fromList [Vector.fromList ".#.", Vector.fromList "###", Vector.fromList ".#."]
footprint Angle  = Vector.fromList [Vector.fromList "..#", Vector.fromList "..#", Vector.fromList "###"]
footprint Pipe   = Vector.fromList [Vector.fromList "#", Vector.fromList "#", Vector.fromList "#", Vector.fromList "#"]
footprint Square = Vector.fromList [Vector.fromList "##", Vector.fromList "##"]

width :: Shape -> Int
width Minus  = 4
width Plus   = 3
width Angle  = 3
width Pipe   = 1
width Square = 2

height :: Shape -> Int
height Minus  = 1
height Plus   = 3
height Angle  = 3
height Pipe   = 4
height Square = 2

data Jet = JLeft | JRight
  deriving (Eq)

data Dir = DUp | DDown | DLeft | DRight

instance Show Jet where
  show JLeft  = "<"
  show JRight = ">"

toDir JLeft = DLeft
toDir JRight = DRight

parseJetPattern :: Parser [Jet]
parseJetPattern = many $ (char '<' >> return JLeft) <|> (char '>' >> return JRight)

type Matrix = Vector (Vector Char)

board :: Matrix -> String
board b = intercalate "\n" (Vector.toList <$> Vector.toList b)

result :: Matrix -> Int
result b = maximum $ length . dropWhile (== '.') <$> transpose (Vector.toList <$> Vector.toList b)

solve :: [Jet] -> Int
solve ps = go 0 Vector.empty (cycle [minBound..]) (cycle ps) 2022
  where
    go h _ _      _  0 = h
    go h v (s:ss) ps n =
      let v'             = insert h s v
          o              = Vector.length v' - h - 3 - height s - 1
          (v'', ps', h') = sim' v' 2 o s ps
      in go (max h h') v'' ss ps' (n - 1)

    coords :: Int -> Int -> Dir -> (Int, Int)
    coords x y DRight = (x + 1, y)
    coords x y DLeft  = (x - 1, y)
    coords x y DDown  = (x, y + 1)
    coords _ _ DUp    = error "should not happen"


    sim v x y s (p:ps) = let ok       = canPlace v x (y + 1) s
                             (x', y') = coords x (y + 1) (toDir p)
                             ok'      = canPlace v x' y' s
                         in case (ok, ok') of
                           (True, True)  -> sim v x' y' s ps <|> Just (x', y', ps)
                           (True, False) -> sim v x (y + 1) s ps <|> Just (x, y + 1, p:ps)
                           (False, _)    -> Just (x, y, p:ps)

    sim' v x y s ps = case sim v x y s ps of
      Nothing            -> error "should not happen"
      Just (x', y', ps') -> let v' = place v x' y' s
                            in (v', ps', Vector.length v' - y')

    needSpace h v s = max (h + height s + 3 - Vector.length v) 0

    vv ! (x, y) = (vv Vector.! y) Vector.! x

    fitHori x s   = 0 <= x && x + width s - 1 < 7

    fitVert y s v = y + height s - 1 < Vector.length v

    isFree (x, y) v = v ! (x, y) == '.'

    canPlace v x y s = let h = height s
                           w = width s
                           pos = [(dx, dy) | dx <- [0..w-1], dy <- [0..h-1]]
                           sv = footprint s
                       in fitHori x s && fitVert y s v && all (uncurry $ canPlace' sv) pos
      where
        canPlace' sv dx dy = let pv   = (x + dx, y + dy)
                                 free = pv `isFree` v
                                 fits = (dx, dy) `isFree` sv
                             in free || fits

    place v x y s
      | canPlace v x y s = let h   = height s
                               w   = width s
                               sv  = footprint s
                           in v Vector.// (map (\y' -> (y + y', placeRow w y' v sv)) [0..h-1])
      | otherwise        = v
      where
        placeRow w y' v sv = let srow = sv Vector.! y'
                                 trow = Vector.slice x w (v Vector.! (y + y'))
                                 merged = Vector.zipWith min srow trow
                             in (v Vector.! (y + y')) Vector.// (zip [x..x+w-1] $ Vector.toList merged)

    insert h s v = let dh = needSpace h v s
                   in (Vector.replicate dh $ Vector.replicate 7 '.') Vector.++ v

main :: IO ()
main = do
  contents <- getContents
  case parse parseJetPattern "" contents of
    Left err      -> print err
    Right pattern -> do
      print $ solve pattern -- 3153

