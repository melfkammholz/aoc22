import Control.Monad (void)
import Data.List (nub, sort)
import Text.Parsec (Parsec, parse, (<|>), skipMany, many1, sepBy1, count, many, optional, try)
import Text.Parsec.Char (string, digit, oneOf, char)

type Point = (Int, Int)
type Interval = (Int, Int)
type Parser = Parsec String ()

data Report = Report Point Point
  deriving (Show, Eq)

sensor :: Report -> Point
sensor (Report s _) = s

beacon :: Report -> Point
beacon (Report _ b) = b

spaces :: Parser ()
spaces = skipMany $ oneOf " \n\r"

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

number :: Parser Int
number = do
  sgn <- (char '-' >> return negate) <|> (return id)
  num <- read <$> many1 digit
  return $ sgn num

report :: Parser Report
report = lexeme $ do
  void $ string "Sensor at x="
  sx <- number
  void $ string ", y="
  sy <- number
  void $ string ": closest beacon is at x="
  bx <- number
  void $ string ", y="
  by <- number
  return $ Report (sx, sy) (bx, by)

reports :: Parser [Report]
reports = report `sepBy1` spaces

solve :: [Report] -> Int -> [Interval]
solve rs y = map clamp $ merge $ sort $ map rema rs'
  where
    rs' = filter (\(Report (sx, sy) b) -> dist (sx, sy) b >= dist (sx, sy) (sx, y)) rs

    rema (Report (sx, sy) b) = let d  = dist (sx, sy) b
                                   d' = dist (sx, sy) (sx, y)
                                   h  = (2 * (d - d' + 1) - 1) `div` 2
                               in (sx - h, sx + h)

    dist (x, y) (x', y') = abs (x - x') + abs (y - y')

    merge []                 = []
    merge [p]                = [p]
    merge ((l, r):(s, t):ps)
      | s <= r    = merge ((l, max r t):ps)
      | otherwise = (l, r) : merge ((s, t):ps)

    clamp (x, y) = (max x 0, min y 4000000)

tuningFreq (x, y) = 4000000 * x + y

main :: IO ()
main = do
  contents <- getContents
  case parse reports "" contents of
    Left err -> print err
    Right rs -> do
      let y = zip (map (solve rs) [0..4000000]) [0..]
      let ([(_,a), _],z) = head $ filter (\(is, _) -> length is == 2) y
      print $ (a + 1, z)
      print $ tuningFreq (a + 1, z)

