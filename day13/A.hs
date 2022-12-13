import Control.Monad (void)
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
morph (Tree ts1)  (Tree ts2)  = let ps   = zipWith morph ts1 ts2
                                    n    = length ps
                                    ts1' = (fst <$> ps) ++ (drop n ts1)
                                    ts2' = (snd <$> ps) ++ (drop n ts2)
                                in (Tree ts1', Tree ts2')

main :: IO ()
main = do
  contents <- getContents
  let (Right ps) = parse pairs "" contents
  let is = map (\(p, i) -> if uncurry (<=) p then i else 0) (zip (uncurry morph <$> ps) [1..])
  print $ sum is

