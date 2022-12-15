import Control.Monad (void)
import Data.List (nub, sort)
import Text.Parsec (Parsec, parse, (<|>), skipMany, many1, sepBy1, count, many, optional, try)
import Text.Parsec.Char (string, digit, oneOf, char)

type Point = (Int, Int)
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

solve rs y = a - b
  where
    rs' = filter (\(Report (sx, sy) b) -> dist (sx, sy) b >= dist (sx, sy) (sx, y)) rs
    rema (Report (sx, sy) b) = let d  = dist (sx, sy) b
                                   d' = dist (sx, sy) (sx, y)
                                   h  = (2 * (d - d' + 1) - 1) `div` 2
                               in (sx - h, sx + h)
    dist (x, y) (x', y') = abs (x - x') + abs (y - y')
    ss = nub $ map (fst . sensor) (filter ((== y) . snd . sensor) rs)
    bs = nub $ map (fst . beacon) (filter ((== y) . snd . beacon) rs)

    a = sum $ map (\(l, r) -> r - l + 1) $ merge $ sort $ map rema rs'
    b = length ss + length bs


    merge []                 = []
    merge [p]                = [p]
    merge ((l, r):(s, t):ps)
      | s <= r    = merge ((l, max r t):ps)
      | otherwise = (l, r) : merge ((s, t):ps)


main :: IO ()
main = do
  contents <- getContents
  case parse reports "" contents of
    Left err -> print err
    Right rs -> do
      print $ solve rs 2000000

