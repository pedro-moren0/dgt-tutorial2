{-# LANGUAGE GADTs #-}

module Grammar.Eval (module Grammar.Eval) where

import Args.HList
import Args.Pickable ((!.))
import Control.Monad (filterM)
import Data.Char (isDigit, isLetter, toLower, toUpper)
import Grammar.Core

type ST a = Either String a

eval :: AST xs a -> Args xs -> ST a
eval (A n) ls = Right $ ls !. n
eval (C (IntL x)) _ = Right x
eval (C (FloatL x)) _ = Right x
eval (C (CharL x)) _ = Right x
eval (C (BoolL x)) _ = Right x
eval (C (PairL x y)) ls = do
  first <- eval (C x) ls
  second <- eval (C y) ls
  return (first, second)
eval (C (ListL xs)) ls = traverse ((`eval` ls) . C) xs
eval (AddInt x1 x2) ls = do
  n <- eval x1 ls
  m <- eval x2 ls
  return $ n + m
eval (SubInt x1 x2) ls = do
  n <- eval x1 ls
  m <- eval x2 ls
  return $ n - m
eval (MultInt x1 x2) ls = do
  n <- eval x1 ls
  m <- eval x2 ls
  return $ n * m
eval (DivInt x1 x2) ls = do
  n <- eval x1 ls
  m <- eval x2 ls
  if m == 0 then Left "Division by zero" else return $ n `div` m
eval (ModInt x1 x2) ls = do
  n <- eval x1 ls
  m <- eval x2 ls
  if m == 0 then Left "Division by zero" else return $ n `mod` m
eval (GTEInt x1 x2) ls = do
  p <- eval x1 ls
  q <- eval x2 ls
  return $ p >= q
eval (LTEInt x1 x2) ls = do
  p <- eval x1 ls
  q <- eval x2 ls
  return $ p <= q
eval (Equals x1 x2) ls = do
  p <- eval x1 ls
  q <- eval x2 ls
  return $ p == q
eval (AddFloat x1 x2) ls = do
  n <- eval x1 ls
  m <- eval x2 ls
  return $ n + m
eval (SubFloat x1 x2) ls = do
  n <- eval x1 ls
  m <- eval x2 ls
  return $ n - m
eval (MultFloat x1 x2) ls = do
  n <- eval x1 ls
  m <- eval x2 ls
  return $ n * m
eval (DivFloat x1 x2) ls = do
  n <- eval x1 ls
  m <- eval x2 ls
  return $ n / m
eval (Sqrt x1) ls = do
  n <- eval x1 ls
  if n < 0 then Left "Negative square root" else return $ sqrt n
eval (IsLetter x1) ls = do
  c <- eval x1 ls
  return $ isLetter c
eval (IsDigit x1) ls = do
  c <- eval x1 ls
  return $ isDigit c
eval (ToUpper x1) ls = do
  c <- eval x1 ls
  return $ toUpper c
eval (ToLower x1) ls = do
  c <- eval x1 ls
  return $ toLower c
eval (And x1 x2) ls = do
  p <- eval x1 ls
  q <- eval x2 ls
  return $ p && q
eval (Or x1 x2) ls = do
  p <- eval x1 ls
  q <- eval x2 ls
  return $ p || q
eval (Not x1) ls = do
  p <- eval x1 ls
  return $ not p
eval (If p x1 x2) ls = do
  cond <- eval p ls
  if cond then eval x1 ls else eval x2 ls
eval (ToPair x1 x2) ls = do
  a <- eval x1 ls
  b <- eval x2 ls
  return (a, b)
eval (Fst x1) ls = do
  pair <- eval x1 ls
  return $ fst pair
eval (Snd x1) ls = do
  pair <- eval x1 ls
  return $ snd pair
eval (Len xs) ls = do
  ys <- eval xs ls
  return $ length ys
eval (Cons x xs) ls = do
  y <- eval x ls
  ys <- eval xs ls
  return $ y : ys
eval (Head xs) ls = do
  ys <- eval xs ls
  return $ head ys
eval (Tail xs) ls = do
  ys <- eval xs ls
  return $ tail ys
eval (Map f xs) ls = do
  ys <- eval xs ls
  traverse (eval f . (:. Nil)) ys
eval (Filter p xs) ls = do
  ys <- eval xs ls
  let cond = eval p . (:. Nil)
  filterM cond ys
