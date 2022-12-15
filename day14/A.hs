import Control.Monad (void, filterM, foldM, foldM_)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector
import Text.Parsec (Parsec, parse, (<|>), skipMany, many1, sepBy1)
import Text.Parsec.Char (string, digit, oneOf)

type Point = (Int, Int)
type Path = [Point]
type Parser = Parsec String ()

spaces :: Parser ()
spaces = skipMany $ oneOf " \n\r"

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

point :: Parser Point
point = lexeme $ do
  x <- read <$> many1 digit
  void $ string ","
  y <- read <$> many1 digit
  return (x, y)

arrow :: Parser String
arrow = lexeme $ string "->"

path :: Parser Path
path = lexeme $ point `sepBy1` arrow

paths :: Parser [Path]
paths = path `sepBy1` spaces

solve :: [Path] -> Int -> IO Int
solve ps sx = do
  mv <- Vector.unsafeThaw $ Vector.replicate (w * h) '.'
  foldM_ (\_ p -> adjust (const '#') mv p) () rs'
  (c, _) <- go mv (sx, 0)
  return c
  where
    go v s = do
      let (x, y) = s
      if y >= h - 3
        then do
          return (0, True)
        else do
          ne <- next s v
          (ss, done) <- foldM
            (\(s, done) n -> if done
                             then return (s, done)
                             else do
                               (s', done') <- go v n
                               return (s + s', done || done'))
            (0, False)
            ne
          adjust (const 'o') v s
          return $ if done
                   then (ss, done)
                   else (1 + ss, done)

    h = (maximum $ concatMap (map snd) ps) + 1 + 2
    w = (maximum $ concatMap (map fst) ps) + 1 + h

    adjust f v (x, y) = MVector.modify v f (y * w + x)

    sel v (x, y) = MVector.read v (y * w + x)

    free g (x, y) = (== '.') <$> sel g (x, y)

    next (x, y) g = filterM (free g) [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

    pointsBetween (x, y) (x', y') =
      let dx = signum $ x' - x
          dy = signum $ y' - y
          n  = max (abs $ x' - x) (abs $ y' - y)
      in (\i -> (x + i * dx, y + i * dy)) <$> [0..n]

    rs = concat $ concatMap (\p -> zipWith pointsBetween p (tail p)) ps
    rs' = rs ++ (pointsBetween (0, h - 1) (w - 1, h - 1))

main :: IO ()
main = do
  contents <- getContents
  case parse paths "" contents of
    Left err -> print err
    Right ps -> do
      s <- solve ps 500
      print s

