import Prelude hiding (round)
import Control.Monad.State
import Control.Monad (void)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.Parsec (Parsec, (<|>), parse, many, many1, manyTill, sepBy, anyChar, oneOf)
import Text.Parsec.Char (string, newline, digit, spaces, letter, char)

type Parser = Parsec String ()

data ThrowTo = ThrowTo Int Int Int
  deriving Show

data Monkey = Monkey Int [Int] (Int -> Int) ThrowTo Int

name :: Monkey -> Int
name (Monkey n _ _ _ _) = n

throws :: Monkey -> Int
throws (Monkey _ _ _ _ t) = t

instance Show Monkey where
  show (Monkey n xs _ tt t) = "Monkey " ++ show n ++ " " ++ show xs ++ " (" ++ show tt ++ ") " ++ show t

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` (char ',' *> spaces)

op :: Parser (Int -> Int)
op = do
  void $ spaces *> string "Operation: new = "
  left <- string "old" <|> many1 digit
  oper <- spaces *> oneOf "+*" <* spaces
  right <- string "old" <|> many1 digit
 
  case (left, right) of
    ("old", "old") -> return $ \x -> mop oper x x
    ("old", _)     -> return (flip (mop oper) $ read right)
    _              -> error "unsupported operation"

  where
    mop '+' = (+)
    mop '*' = (*)
    mop _   = error "unsupported operator"

test :: Parser ThrowTo
test = do
  void $ spaces *> string "Test: divisible by "
  n <- many1 digit
  void $ spaces *> string "If true: throw to monkey "
  m1 <- many1 digit
  void $ spaces *> string "If false: throw to monkey "
  m2 <- many1 digit
  return $ ThrowTo (read n) (read m1) (read m2)

items :: Parser [Int]
items = do
  void $ spaces *> string "Starting items: "
  xs <- commaSep (many1 digit)
  return $ map read xs

makeTest :: ThrowTo -> Int -> Int
makeTest (ThrowTo n a b) x = if x `mod` n == 0 then a else b

monkey :: Parser Monkey
monkey = do
  void $ string "Monkey "
  n <- many1 digit
  void $ string ":"
  xs <- items
  op' <- op
  test' <- test
  void $ spaces
  return $ Monkey (read n) xs op' test' 0

monkeys :: Parser [Monkey]
monkeys = many monkey

round :: [Monkey] -> [Monkey]
round ms = let ns = name <$> ms
               st = Map.fromList $ zip ns ms
         in map snd $ Map.toList $ go ns st
  where
    go []     m = m
    go (n:ns) m =
      let (Monkey _ xs op test t) = fromJust $ Map.lookup n m
          m' = Map.insert n (Monkey n [] op test (t + length xs)) m
          throw x = Map.adjust (add $ op x) (makeTest test $ op x)
          m'' = foldr throw m' xs 
      in go ns m''

    add x (Monkey n xs op test t) = Monkey n (x : xs) op test t

main :: IO ()
main = do
  contents <- getContents
  case parse monkeys "" contents of
    Left e -> print e
    Right ms -> do
      let p    = product $ map (\(Monkey _ _ _ (ThrowTo d _ _) _) -> d) ms
      let wl   = (`mod` p)
      let ms'  = map (\(Monkey n xs op tt t) -> Monkey n xs (wl . op) tt t) ms
      let ms'' = foldl1 (.) (replicate 10000 round) $ ms'
      let ts   = reverse . sort $ throws <$> ms''
      print $ product $ take 2 ts

