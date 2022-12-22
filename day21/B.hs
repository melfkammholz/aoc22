import Data.Either (fromRight)
import Data.Map (Map)
import qualified Data.Map as Map
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

-- partially evaluates tree
unsafeEval :: String -> Map String (Either Node Integer) -> Map String (Either Node Integer)
unsafeEval s m = go s m
  where
    go s m =
      let n = m Map.! s
      in case n of
        Right _              -> m
        Left (Num "humn" x)  -> m
        Left (Num _ x)       -> Map.insert s (Right x) m
        Left (Node _ l op r) ->
          let m'   = go l m
              m''  = go r m'
              elv  = m'' Map.! l
              erv  = m'' Map.! r
          in case (elv, erv) of
            (Right lv, Right rv) -> case op of
              Plus  -> Map.insert s (Right $ lv + rv) m''
              Minus -> Map.insert s (Right $ lv - rv) m''
              Mult  -> Map.insert s (Right $ lv * rv) m''
              Div   -> Map.insert s (Right $ lv `div` rv) m''
            (_, _)               -> m''


buildMap :: [Node] -> Map String (Either Node Integer)
buildMap = Map.fromList . map toTuple
  where
    toTuple n@(Num lb _)      = (lb, Left n)
    toTuple n@(Node lb _ _ _) = (lb, Left n)

binarySearch :: Map String (Either Node Integer) -> Integer -> Integer -> Integer
binarySearch m l r =
  let mi = l + (r - l) `div` 2
  in if l >= r
    then if monoInc then r else l
    else if check mi
      then binarySearch m l mi
      else binarySearch m (mi + 1) r
  where
    eval x = let m'  = Map.insert "humn" (Right x) m
                 m'' = unsafeEval "root" m'
                 (Left (Node _ l _ r)) = m Map.! "root"
                 lv  = right $ m'' Map.! l
                 rv  = right $ m'' Map.! r
             in (lv, rv)

    monoInc = let (lv1, rv1) = eval 1
                  (lv2, rv2)  = eval 10000000000
              in lv1 <= lv2 && rv1 <= rv2

    check mi = let (lv, rv) = eval mi
               in if monoInc
                    then lv >= rv
                    else lv <= rv

main :: IO ()
main = do
  ls <- lines <$> getContents
  case sequence $ parse node "" <$> ls of
    Left err -> print err
    Right ns -> do
      let m  = buildMap ns
      print $ binarySearch (unsafeEval "root" m) 0 (toInteger (maxBound :: Int))


