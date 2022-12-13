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

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` (char ',' *> spaces)

leaf :: Parser Tree
leaf = Leaf . read <$> many1 digit

tree :: Parser Tree
tree = Tree <$> (string "[" *> commaSep (leaf <|> tree) <* string "]")

trees :: Parser [Tree]
trees = (tree <* spaces) `sepBy` spaces

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
  let (Right ps) = parse trees "" contents
  let t1 = Tree [Tree [Leaf 2]]
  let t2 = Tree [Tree [Leaf 6]]
  let ps' = t1 : t2 : ps
  let ps'' = sortBy (curry $ uncurry compare . uncurry morph) ps'
  print $ fromJust $ (*) <$> ((+1) <$> findIndex (== t1) ps'') <*> ((+1) <$> findIndex (== t2) ps'')

