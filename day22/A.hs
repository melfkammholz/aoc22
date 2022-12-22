{-# LANGUAGE TupleSections #-}
import Data.List (groupBy)
import Data.Maybe (maybe, isNothing, fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec (Parsec, parse, many1, manyTill, skipMany, oneOf, (<|>), try)
import Text.Parsec.Char (string, digit, char, letter)

type Move = (Char, Int)
type Grid = Vector (Vector Char)

type Parser = Parsec String ()

spaces :: Parser ()
spaces = skipMany $ oneOf " \n\r"

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

parseGrid :: Parser Grid
parseGrid = lexeme $ do
  ls <- lines <$> manyTill (oneOf " .#\n") (try $ string "\n\n")
  return $ Vector.fromList $ Vector.fromList <$> ls


number :: Parser Int
number = read <$> many1 digit

parseMoves :: Parser [Move]
parseMoves = lexeme $ do
  f  <- number
  ms <- many1 $ (,) <$> oneOf "LR" <*> number
  return $ ('R', f) : ms

parseInput :: Parser (Grid, [Move])
parseInput = (,) <$> parseGrid <*> parseMoves

solve :: Grid -> [Move] -> Int
solve g ms = password $ go (sx, sy) 3 ms buildJumpTable
  where
    go :: (Int, Int) -> Int -> [Move] -> Map (Int, Int, Int) (Int, Int, Int) -> (Int, Int, Int)
    go (x, y) f []           jt = (x, y, f)
    go (x, y) f (m@(r,s):ms) jt = let f' = turn r f
                                      (mx, my, _) = move (x, y) f' s jt
                                  in go (mx, my) f' ms jt

    password (c, r, f) = (r + 1) * 1000 + (c + 1) * 4 + f

    turn 'L' f = (f - 1) % 4
    turn 'R' f = (f + 1) % 4

    move (x, y) d 0 jt = (x, y, d)
    move (x, y) d s jt = let (dx, dy) = dir d
                             wa       = canStand (x + dx, y + dy)
                             oo       = onOutline d (x, y)
                         in case (wa, oo) of
                           (True,  _)     -> move (x + dx, y + dy) d (s - 1) jt
                           (False, False) -> (x, y, d)
                           (False, True)  -> maybe (x, y, d) (\(nx, ny, _) -> move (nx, ny) d (s - 1) jt) (jt Map.!? (x, y, d))

    (sx, sy) = head $ filter canStand [(x, y) | y <- [0..height - 1], x <- [0..width - 1]]

    isWalk p = maybe False (== '#') (see p)
    canStand p = maybe False (== '.') (see p)

    n % m = (n `mod` m + m) `mod` m

    dir 0 = (1, 0)
    dir 1 = (0, 1)
    dir 2 = (-1, 0)
    dir 3 = (0, -1)
    dir n = dir $ (n % 4)

    see (x, y) = g Vector.!? y >>= \r -> r Vector.!? x

    outOfBounds (x, y) = maybe True (== ' ') $ see (x, y)
    onOutline f (x, y) = let (dx, dy) = dir f
                             mc = see (x, y)
                             mp = see (x - dx, y - dy)
                             mn = see (x + dx, y + dy)
                         in case mc of
                           Nothing -> False
                           Just c  -> not (isVoid c) && case (mp, mn) of
                             (Nothing, _) -> True
                             (_, Nothing) -> True
                             (Just pc, Just nc) -> isVoid pc || isVoid nc

    isVoid c = c == ' '

    width = foldr (\r m -> max m (Vector.length r)) 0 g

    height = Vector.length g

    buildJumpTable = Map.fromList $ filter (uncurry validJump)
                                  $ concatMap (\f -> let forw = concatMap group2 $ jumpTable f in forw ++ (map back forw)) [0, 1]
      where
        firstEnd []     = []
        firstEnd (x:xs) = xs ++ [x]

        validJump (x, y, _) (u, v, _) = fromJust $ do
          w1 <- see (x, y)
          w2 <- see (u, v)
          return $ w1 /= '#' && w2 /= '#'

        genPoints 0 = [(x, y) | y <- [0..height - 1], x <- [0..width - 1]]
        genPoints 1 = [(x, y) | x <- [0..width - 1], y <- [0..height - 1]]
        genPoints n = genPoints (n % 2)

        jumpTable f =
          map (firstEnd . map (uncurry (,,f)))
          $ groupBy ((\s p q -> s p == s q) $ [snd, fst, snd, fst] !! (f % 4))
          $ filter ((&&) <$> not . outOfBounds <*> onOutline f)
          $ genPoints f

        group2 []       = []
        group2 [_]      = []
        group2 (x:y:xs) = (x, y) : group2 (y:xs)

        back ((x,y,f), (u,v,_)) = ((u,v,(f+2)%4), (x,y,(f+2)%4))


main :: IO ()
main = do
  inp <- parse parseInput "" <$> getContents
  case inp of
    Left err -> print err
    Right (g, ms) -> do
      print $ solve g ms
