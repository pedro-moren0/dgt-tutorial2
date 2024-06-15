{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grammar.Core2 (module Grammar.Core2) where

import Data.Kind
import Data.Proxy
import Data.Void
import GHC.TypeNats
import HList
import MyNat

-- data Type' where
--   IntT' :: Type'
--   FloatT' :: Type'
--   CharT' :: Type'
--   BoolT' :: Type'
--   PolyT' :: Int -> Type'
--   PairT' :: Type' -> Type' -> Type'
--   ListT' :: Type' -> Type'
--   LambdaT' :: FunctionT' -> Type'

-- deriving instance Eq Type'

-- data FunctionT' = FunctionT' {_argT' :: ArgT', _outT' :: OutT'} deriving (Eq)

-- type ArgT' = [Type']

-- type OutT' = Type'

data Lit' a where
  -- ArgL' :: Int -> Lit' a
  -- LambdaL' :: TAST' a -> Lit' a
  IntL' :: Int -> Lit' Int
  FloatL' :: Float -> Lit' Float
  CharL' :: Char -> Lit' Char
  BoolL' :: Bool -> Lit' Bool
  PairL' :: Lit' b -> Lit' c -> Lit' (b, c)
  ListL' :: [Lit' a] -> Lit' [a]

-- x :: Op' Int '[Int]
-- x = A $ natVal 9

type Op' :: Type -> [Type] -> Type
data Op' a xs where
  A :: (KnownNat n, n < Len xs, Pickable xs n) => SNat n -> Op' (xs ! n) xs
  C :: Lit' a -> Op' a xs
  AddInt' :: Op' Int xs -> Op' Int xs -> Op' Int xs
  SubInt' :: Op' Int xs -> Op' Int xs -> Op' Int xs
  MultInt' :: Op' Int xs -> Op' Int xs -> Op' Int xs
  DivInt' :: Op' Int xs -> Op' Int xs -> Op' Int xs
  ModInt' :: Op' Int xs -> Op' Int xs -> Op' Int xs
  GTEInt' :: Op' Int xs -> Op' Int xs -> Op' Bool xs
  LTEInt' :: Op' Int xs -> Op' Int xs -> Op' Bool xs
  Equals :: (Eq a) => Op' a xs -> Op' a xs -> Op' Bool xs
  AddFloat' :: Op' Float xs -> Op' Float xs -> Op' Float xs
  SubFloat' :: Op' Float xs -> Op' Float xs -> Op' Float xs
  MultFloat' :: Op' Float xs -> Op' Float xs -> Op' Float xs
  DivFloat' :: Op' Float xs -> Op' Float xs -> Op' Float xs
  Sqrt' :: Op' Float xs -> Op' Float xs
  IsLetter' :: Op' Char xs -> Op' Bool xs
  IsDigit' :: Op' Char xs -> Op' Bool xs
  ToUpper' :: Op' Char xs -> Op' Char xs
  ToLower' :: Op' Char xs -> Op' Char xs
  And' :: Op' Bool xs -> Op' Bool xs -> Op' Bool xs
  Or' :: Op' Bool xs -> Op' Bool xs -> Op' Bool xs
  Not' :: Op' Bool xs -> Op' Bool xs
  If' :: Op' Bool xs -> Op' a xs -> Op' a xs -> Op' a xs
  ToPair' :: Op' a xs -> Op' b xs -> Op' (a, b) xs
  Fst' :: Op' (a, b) xs -> Op' a xs
  Snd' :: Op' (a, b) xs -> Op' b xs
  Len' :: Op' [a] xs -> Op' Int xs
  Cons' :: Op' a xs -> Op' [a] xs -> Op' [a] xs
  Head' :: Op' [a] xs -> Op' a xs
  Tail' :: Op' [a] xs -> Op' [a] xs

--   Map' :: (Op' a -> Op' b) -> Op' [a] -> Op' [b]
--   Filter' :: (Op' a -> Op' Bool) -> Op' [a] -> Op' [a]

-- data Terminal' a = Literal' (Lit' a) | Arg' Int

-- data AST' a = Leaf' (Terminal' a) | Node' (Op' a)
type AST' a xs = Op' a xs

type Cnst :: [Type] -> Constraint
type family Cnst a where
  Cnst a = ()

-- data TAST' a = TAST' {_ast' :: AST' a, _ftype' :: FunctionT'}