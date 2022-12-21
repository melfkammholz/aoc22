import Control.Exception (assert)
import Data.Either (fromRight)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (traceShowId, trace)
import Text.Parsec (Parsec, parse, many1, manyTill, skipMany, oneOf, (<|>), try)
import Text.Parsec.Char (string, digit, char, letter)

data Op = Plus | Mult | Div | Minus
  deriving (Show, Eq)

data Node = Node String String Op String | Num String Integer
  deriving (Show, Eq)

type Parser = Parsec String ()

spaces :: Parser ()
spaces = skipMany $ oneOf " \n\r"

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

node :: Parser Node
node = try eq <|> num
  where
    lbl = lexeme $ many1 letter
    num = Num <$> (lbl <* string ": ")
              <*> (read <$> (lexeme $ many1 digit))
    op  = lexeme $ (char '*' *> return Mult)
               <|> (char '/' *> return Div)
               <|> (char '+' *> return Plus)
               <|> (char '-' *> return Minus)
    eq  = Node <$> (lbl <* string ": ")
               <*> lbl
               <*> op
               <*> lbl

right :: Either a b -> b
right = fromRight (error "should not happen")

unsafeEval :: String -> Map String (Either Node Integer) -> Map String (Either Node Integer)
unsafeEval s m = go s m
  where
    go :: String -> Map String (Either Node Integer) -> Map String (Either Node Integer)
    go s m =
      let n = m Map.! s
      in case n of
        Left (Num _ x)       -> Map.insert s (Right x) m
        Left (Node _ l op r) ->
          let m'  = go l m
              m'' = go r m'
              lv  = right $ m'' Map.! l
              rv  = right $ m'' Map.! r
          in case op of
            Plus  -> Map.insert s (Right $ lv + rv) m''
            Minus -> Map.insert s (Right $ lv - rv) m''
            Mult  -> Map.insert s (Right $ lv * rv) m''
            Div   -> Map.insert s (Right $ lv `div` rv) m''


buildMap :: [Node] -> Map String (Either Node Integer)
buildMap = Map.fromList . map toTuple
  where
    toTuple n@(Num lb _)      = (lb, Left n)
    toTuple n@(Node lb _ _ _) = (lb, Left n)

main :: IO ()
main = do
  ls <- lines <$> getContents
  case sequence $ parse node "" <$> ls of
    Left err -> print err
    Right ns -> do
      let m  = buildMap ns
      let (Right res) = (unsafeEval "root" m) Map.! "root"
      print res

