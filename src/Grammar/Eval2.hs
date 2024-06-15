{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Grammar.Eval2 (module Grammar.Eval2) where

import Data.Char (isDigit, isLetter, toLower, toUpper)
import Grammar.Core2
import HList
import MyNat

eval' :: AST' a xs -> HList xs -> a
eval' (A n) ls = ls !. n
eval' (C (IntL' x)) _ = x
eval' (C (FloatL' x)) _ = x
eval' (C (CharL' x)) _ = x
eval' (C (BoolL' x)) _ = x
eval' (C (PairL' x y)) ls = (eval' (C x) ls, eval' (C y) ls)
eval' (C (ListL' xs)) ls = (`eval'` ls) . C <$> xs
eval' (AddInt' x1 x2) ls = eval' x1 ls + eval' x2 ls
eval' (SubInt' x1 x2) ls = eval' x1 ls - eval' x2 ls
eval' (MultInt' x1 x2) ls = eval' x1 ls * eval' x2 ls
eval' (DivInt' x1 x2) ls = eval' x1 ls `div` eval' x2 ls -- falha
eval' (ModInt' x1 x2) ls = eval' x1 ls `mod` eval' x2 ls -- falha
eval' (GTEInt' x1 x2) ls = eval' x1 ls >= eval' x2 ls
eval' (LTEInt' x1 x2) ls = eval' x1 ls <= eval' x2 ls
eval' (Equals x1 x2) ls = eval' x1 ls == eval' x2 ls
eval' (AddFloat' x1 x2) ls = eval' x1 ls + eval' x2 ls
eval' (SubFloat' x1 x2) ls = eval' x1 ls - eval' x2 ls
eval' (MultFloat' x1 x2) ls = eval' x1 ls * eval' x2 ls
eval' (DivFloat' x1 x2) ls = eval' x1 ls / eval' x2 ls
eval' (Sqrt' x1) ls = sqrt $ eval' x1 ls -- falha
eval' (IsLetter' x1) ls = isLetter $ eval' x1 ls
eval' (IsDigit' x1) ls = isDigit $ eval' x1 ls
eval' (ToUpper' x1) ls = toUpper $ eval' x1 ls
eval' (ToLower' x1) ls = toLower $ eval' x1 ls
eval' (And' x1 x2) ls = eval' x1 ls && eval' x2 ls
eval' (Or' x1 x2) ls = eval' x1 ls || eval' x2 ls
eval' (Not' x1) ls = not $ eval' x1 ls
eval' (If' p x1 x2) ls = if eval' p ls then eval' x1 ls else eval' x2 ls
eval' (ToPair' x1 x2) ls = (eval' x1 ls, eval' x2 ls)
eval' (Fst' x1) ls = fst $ eval' x1 ls
eval' (Snd' x1) ls = snd $ eval' x1 ls
eval' (Len' xs) ls = length $ eval' xs ls
eval' (Cons' x xs) ls = eval' x ls : eval' xs ls
eval' (Head' xs) ls = head $ eval' xs ls -- falha
eval' (Tail' xs) ls = tail $ eval' xs ls

-- evalAST' :: AST' a -> a
-- evalAST' ast = eval' ast []