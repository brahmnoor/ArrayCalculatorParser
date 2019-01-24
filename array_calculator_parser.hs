-- Author - Brahmnoor Singh Chawla
import Data.Char
import Data.List
import System.IO
import Parsing

data Op = Add | Sub | Div
  deriving (Eq, Show)

singleOp :: Op -> Maybe Int -> Maybe Int -> Maybe Int
singleOp Add (Just x) (Just y) = Just (x + y)
singleOp Sub (Just x) (Just y) = Just (x - y)
singleOp Div (Just x) (Just 0) = Nothing
singleOp Div (Just x) (Just y) = Just (x `div` y)

join :: Maybe Int -> Maybe [Int] -> Maybe [Int]
join Nothing   _          = Nothing
join _         Nothing    = Nothing
join (Just x)  (Just xs)  = Just (x:xs)

applyOp :: Op -> Maybe [Int] -> Maybe [Int] -> Maybe [Int]
applyOp _ Nothing       _             = Nothing
applyOp _ _             Nothing       = Nothing
applyOp _ (Just [])     (Just (y:ys)) = Nothing
applyOp _ (Just (x:xs)) (Just [])     = Nothing
applyOp _ (Just [])     (Just [])     = Just []
applyOp o (Just (x:xs)) (Just (y:ys)) = join (singleOp o (Just x) (Just y)) (applyOp o (Just xs) (Just ys))

data Expr
  = Bin Op Expr Expr
  | Val [Int]
  | Var String
  | IsNotAnExpr
  deriving (Eq, Show)

type Env = [(String, [Int])]

eval :: Env -> Expr -> Maybe [Int]
eval en (Val xs) = Just xs
eval en (Var s) = lookup s en
eval en (Bin o x y) = applyOp o (eval en x) (eval en y)

some_ints :: Parser [Int]
some_ints = notEmpty +++ return []
  where
    notEmpty = do
      char ','
      x <- integer
      xs <- some_ints
      return (x:xs)

pList :: Parser Expr
pList = do
  token $ char '['
  ys <- notEmpty +++ return []
  token $ char ']'
  return (Val ys)
  where
    notEmpty = do
      x <- integer
      xs <- some_ints
      return (x:xs)

pIdentifierRemaining :: Parser String
pIdentifierRemaining = notEmpty +++ return ""
  where
    notEmpty = do
      x <- sat isAlphaNum
      xs <- pIdentifierRemaining
      return (x:xs)

pIdentifier :: Parser Expr
pIdentifier = do
  x <- token $ sat isLower
  xs <- pIdentifierRemaining
  return (Var (x:xs))

pIdentifierStr :: Parser String
pIdentifierStr = do
  x <- token $ sat isLower
  xs <- pIdentifierRemaining
  return ((x:xs))


pFactor :: Parser Expr
pFactor = isExpr +++ pList +++ pIdentifier
  where
    isExpr = do
      token $ char '('
      x <- pExpr
      token $ char ')'
      return x

pOpFactor :: Parser [Expr]
pOpFactor = notEmpty +++ return []
  where
    notEmpty = do
      token $ char '/'
      x <- pFactor
      xs <- pOpFactor
      return (x:xs)

exprToBin :: Op -> Expr -> Expr -> Expr
exprToBin _ x IsNotAnExpr = x
exprToBin _ IsNotAnExpr x = x
exprToBin o x y = Bin o x y

pTerm :: Parser Expr
pTerm = do
  x <- pFactor
  xs <- pOpFactor
  if (length xs /= 0) then return (foldl (exprToBin Div) IsNotAnExpr (x:xs)) else return x

pOpTerm :: Parser [(Op, Expr)]
pOpTerm = notEmpty +++ return []
  where
    notEmpty = do
      x <- token ( char '+' ) +++ token ( char '-' )
      y <- pTerm
      xs <- pOpTerm
      if (x == '+') then return ((Add, y):xs) else
        if (x == '-') then return ((Sub, y):xs) else
          return []

finalExprToBin :: Expr -> [(Op, Expr)] -> Expr
finalExprToBin x [] = x
finalExprToBin x ((o, y):xs) = let z = exprToBin o x y
                               in finalExprToBin z xs

pExpr :: Parser Expr
pExpr = do
  x <- pTerm
  xs <- pOpTerm
  if (length xs /= 0) then return (finalExprToBin x xs) else return x

runParser :: Parser a -> String -> Maybe a
runParser (P p) inp
  | length x == 0 = Nothing
  | length (snd (head x)) /= 0 = Nothing
  | otherwise = Just (fst (head (x)))
  where
    x = p inp
