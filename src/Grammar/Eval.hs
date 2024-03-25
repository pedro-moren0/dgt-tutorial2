module Grammar.Eval (module Grammar.Eval) where

import Data.Char (isAlpha, isDigit, toLower, toUpper)
import Grammar.Core
  ( AST (..),
    FunctionT (..),
    Lit (..),
    Op (..),
    TAST (..),
    Terminal (..),
  )
import Grammar.Utils (ariOp, tipoLit)

type StackTrace = String

eval :: Op -> [Lit] -> Either StackTrace Lit
eval AddInt [IntL x1, IntL x2] = Right $ IntL (x1 + x2)
eval SubInt [IntL x1, IntL x2] = Right $ IntL (x1 - x2)
eval MultInt [IntL x1, IntL x2] = Right $ IntL (x1 * x2)
eval op@ModInt args@[IntL x1, IntL x2]
  | x2 == 0 = Left $ makeFullStacktrace op args "\nDivision by zero!"
  | otherwise = Right $ IntL (x1 `mod` x2)
eval LTEInt [IntL x1, IntL x2] = Right $ BoolL (x1 <= x2)
eval EqChar [IntL x1, IntL x2] = Right $ BoolL (x1 == x2)
eval AddFloat [FloatL x1, FloatL x2] = Right $ FloatL (x1 + x2)
eval SubFloat [FloatL x1, FloatL x2] = Right $ FloatL (x1 - x2)
eval MultFloat [FloatL x1, FloatL x2] = Right $ FloatL (x1 * x2)
eval op@Sqrt args@[FloatL x]
  | x < 0 = Left $ makeFullStacktrace op args "\nNo square root for negative numbers!"
  | otherwise = Right $ FloatL (sqrt x)
eval EqChar [CharL c1, CharL c2] = Right $ BoolL (c1 == c2)
eval EqInt [IntL x1, IntL x2] = Right $ BoolL (x1 == x2)
eval IsLetter [CharL c] = Right $ BoolL (isAlpha c)
eval IsDigit [CharL c] = Right $ BoolL (isDigit c)
eval ToUpper [CharL c] = Right $ CharL (toUpper c)
eval ToLower [CharL c] = Right $ CharL (toLower c)
eval And [BoolL p, BoolL q] = Right $ BoolL (p && q)
eval Or [BoolL p, BoolL q] = Right $ BoolL (p || q)
eval Not [BoolL p] = Right $ BoolL (not p)
eval If [BoolL cond, a, b] = Right $ if cond then a else b
eval ToPair [a, b] = Right $ PairL a b
eval Fst [PairL a _] = Right a
eval Snd [PairL _ b] = Right b
eval Len [ListL _ xs] = Right $ IntL (length xs)
eval op@Cons args@[x, ListL t xs]
  | t /= tipoLit x = Left $ makeFullStacktrace op args "\nType mismatch between "
  | otherwise = Right $ ListL t (x : xs)
eval op@Head args@[ListL _ xs]
  | null xs = Left $ makeFullStacktrace op args "\nCan't get first element of empty list!"
  | otherwise = Right (head xs)
eval Tail [ListL t xs] = Right $ ListL t (tail xs)
eval op@Map args@[LambdaL tast, ListL _ xs] = case stOrLits of
  Right lits -> Right $ ListL ((_outT . _ftype) tast) lits
  Left st -> Left $ makeFullStacktrace op args st
  where
    stOrLits :: Either StackTrace [Lit]
    stOrLits = sequenceA (xs >>= \x -> pure $ evalAST [x] (_ast tast))
eval op@Filter args@[LambdaL tast, ListL t xs] = case stOrLits of
  Right lits -> Right $ ListL t lits
  Left st -> Left $ makeFullStacktrace op args st
  where
    stOrLits :: Either StackTrace [Lit]
    stOrLits = sequenceA $ do
      x <- xs
      let stOrLit = evalAST (pure x) (_ast tast)
      let res = [] :: [Either StackTrace Lit]
      case stOrLit of
        Right (BoolL b) -> if b then Right x : res else res
        Right _ -> res
        Left st -> Left (makeFullStacktrace op args st) : res
eval op args
  | null args = Left $ makeFullStacktrace op args "\nNo arguments provided!"
  | argsLen < ariOp op = Left $ makeFullStacktrace op args "\nToo little arguments provided!"
  | argsLen > ariOp op = Left $ makeFullStacktrace op args "\nToo many arguments provided!"
  | otherwise = Left $ makeFullStacktrace op args ""
  where
    argsLen = length args

evalAST :: [Lit] -> AST -> Either StackTrace Lit
evalAST args (Leaf (Arg argNum)) =
  if argNum < argsLen
    then Right $ args !! argNum
    else
      Left $
        "\nInvalid access to provided arguments.\nYou tried to access the "
          <> show (argNum + 1)
          <> ordAbrv (argNum + 1)
          <> " argument when "
          <> show argsLen
          <> " arguments were provided"
  where
    argsLen = length args

    ordAbrv :: Int -> String
    ordAbrv 1 = "st"
    ordAbrv 2 = "nd"
    ordAbrv 3 = "rd"
    ordAbrv _ = "th"
evalAST _ (Leaf (Literal lit)) = Right lit
evalAST args (Node op asts) = case traverse (evalAST args) asts of
  Right lits -> eval op lits
  Left st -> Left $ putOpInStacktrace op st

putLitsInStacktrace :: [Lit] -> StackTrace -> StackTrace
putLitsInStacktrace lits st = st <> "\nError: args " <> show lits

putOpInStacktrace :: Op -> StackTrace -> StackTrace
putOpInStacktrace op st = st <> "\nAt: " <> show op

makeFullStacktrace :: Op -> [Lit] -> StackTrace -> StackTrace
makeFullStacktrace op lits = putOpInStacktrace op . putLitsInStacktrace lits

printLitST :: Either StackTrace Lit -> IO ()
printLitST (Left st) = putStrLn st
printLitST (Right lit) = print lit