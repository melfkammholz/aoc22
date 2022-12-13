import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.List (findIndex, sortBy)
import Data.Maybe (fromJust)
import Text.Parsec (Parsec, parse, (<|>), skipMany, many1, sepBy)
import Text.Parsec.Char (string, digit, char, oneOf)

data Tree = Tree [Tree] | Leaf Int
  deriving (Show, Eq, Ord)

type Parser = Parsec String ()

spaces :: Parser ()
spaces = skipMany $ oneOf " \n\r"

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` (lexeme $ char ',')

leaf :: Parser Tree
leaf = Leaf . read <$> (lexeme $ many1 digit)

tree :: Parser Tree
tree = Tree <$> (lexeme $ string "[" *> commaSep (leaf <|> tree) <* string "]")

pair :: Parser (Tree, Tree)
pair = do
  t1 <- tree
  t2 <- tree
  return (t1, t2)

pairs :: Parser [(Tree, Tree)]
pairs = pair `sepBy` spaces

morph :: Tree -> Tree -> (Tree, Tree)
morph l1@(Leaf _) l2@(Leaf _) = (l1, l2)
morph (Tree ts1)  l2@(Leaf _) = morph (Tree ts1) (Tree [l2])
morph l1@(Leaf _) (Tree ts2)  = morph (Tree [l1]) (Tree ts2)
morph (Tree ts1)  (Tree ts2)  = bimap Tree Tree (zipzap morph ts1 ts2)
  where
    zipzap _ xs     []     = (xs, [])
    zipzap _ []     ys     = ([], ys)
    zipzap f (x:xs) (y:ys) = let (x', y') = f x y
                             in bimap (x':) (y':) (zipzap f xs ys)

main :: IO ()
main = do
  contents <- getContents
  let (Right ps) = parse pairs "" contents
  let is = snd <$> filter (uncurry (<=) . fst) (zip (uncurry morph <$> ps) [1..])
  print $ sum is

