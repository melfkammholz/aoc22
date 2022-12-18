import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Text.Parsec (Parsec, parse, many1, sepBy1, oneOf, skipMany)
import Text.Parsec.Char (char, digit)

type Parser = Parsec String ()

spaces :: Parser ()
spaces = skipMany $ oneOf " \n\r"

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

type Lava = (Int, Int, Int)

parseLava :: Parser Lava
parseLava = lexeme $ do
  [x, y, z] <- (many1 digit) `sepBy1` (lexeme $ char ',')
  return (read x, read y, read z)

parseLavas :: Parser [Lava]
parseLavas = parseLava `sepBy1` spaces

solve ls = sum $ map (\l -> length $ filter (not . isLava) $ around l) ls
  where
    hls = HashSet.fromList ls

    isLava l = HashSet.member l hls
    around (x, y, z) = [(x + dx, y + dy, z + dz) | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], (dx, dy, dz) /= (0, 0, 0), abs dx + abs dy + abs dz == 1]

main :: IO ()
main = do
  input <- getContents
  case parse parseLavas "" input of
    Left err -> print err
    Right ls -> do
      print $ solve ls
