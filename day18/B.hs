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

solve :: [Lava] -> Int
solve ls = surf (HashSet.difference p hls) - surf inn
  where
    hls = HashSet.fromList ls
    inn = HashSet.difference (erase (mix-1, miy-1, miz-1) p) hls

    surf = sum . map (length . filter isLava . around) . HashSet.toList

    fold' _ _ _ t         []             = t
    fold' f g h (a, b, c) ((x, y, z):xs) = fold' f g h (f x a, g y b, h z c) xs

    (mix, miy, miz) = fold' min min min (20, 20, 20) ls
    (mx, my, mz) = fold' max max max (0, 0, 0) ls

    p = HashSet.fromList [(x, y, z) | x <- [mix-1..mx+1], y <- [miy-1..my+1], z <- [miz-1..mz+1]]

    erase (x, y, z) p = foldr erase (del p as) as
      where
        as = filter (\l -> not (isLava l) && HashSet.member l p) (around (x, y, z))
        del = foldr HashSet.delete

    isLava = flip HashSet.member hls
    around (x, y, z) = [(x + dx, y + dy, z + dz) | dx <- [-1..1],
                                                   dy <- [-1..1],
                                                   dz <- [-1..1],
                                                   (dx, dy, dz) /= (0, 0, 0),
                                                   abs dx + abs dy + abs dz == 1]

main :: IO ()
main = do
  input <- getContents
  case parse parseLavas "" input of
    Left err -> print err
    Right ls -> do
      print $ solve ls
