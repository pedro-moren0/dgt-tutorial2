{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Grammar.Core (module Grammar.Core) where

import Args.FamilyUtils
import Args.Pickable
import Data.Kind (Type)
import GHC.TypeNats

data Lit a where
  IntL :: Int -> Lit Int
  FloatL :: Float -> Lit Float
  CharL :: Char -> Lit Char
  BoolL :: Bool -> Lit Bool
  PairL :: Lit b -> Lit c -> Lit (b, c)
  ListL :: [Lit a] -> Lit [a]

type Op :: [Type] -> Type -> Type
data Op xs a where
  A :: (KnownNat n, n < Len xs, Pickable xs n) => SNat n -> Op xs (xs ! n)
  C :: Lit a -> Op xs a
  AddInt :: Op xs Int -> Op xs Int -> Op xs Int
  SubInt :: Op xs Int -> Op xs Int -> Op xs Int
  MultInt :: Op xs Int -> Op xs Int -> Op xs Int
  DivInt :: Op xs Int -> Op xs Int -> Op xs Int
  ModInt :: Op xs Int -> Op xs Int -> Op xs Int
  GTEInt :: Op xs Int -> Op xs Int -> Op xs Bool
  LTEInt :: Op xs Int -> Op xs Int -> Op xs Bool
  Equals :: (Eq a) => Op xs a -> Op xs a -> Op xs Bool
  AddFloat :: Op xs Float -> Op xs Float -> Op xs Float
  SubFloat :: Op xs Float -> Op xs Float -> Op xs Float
  MultFloat :: Op xs Float -> Op xs Float -> Op xs Float
  DivFloat :: Op xs Float -> Op xs Float -> Op xs Float
  Sqrt :: Op xs Float -> Op xs Float
  IsLetter :: Op xs Char -> Op xs Bool
  IsDigit :: Op xs Char -> Op xs Bool
  ToUpper :: Op xs Char -> Op xs Char
  ToLower :: Op xs Char -> Op xs Char
  And :: Op xs Bool -> Op xs Bool -> Op xs Bool
  Or :: Op xs Bool -> Op xs Bool -> Op xs Bool
  Not :: Op xs Bool -> Op xs Bool
  If :: Op xs Bool -> Op xs a -> Op xs a -> Op xs a
  ToPair :: Op xs a -> Op xs b -> Op xs (a, b)
  Fst :: Op xs (a, b) -> Op xs a
  Snd :: Op xs (a, b) -> Op xs b
  Len :: Op xs [a] -> Op xs Int
  Cons :: Op xs a -> Op xs [a] -> Op xs [a]
  Head :: Op xs [a] -> Op xs a
  Tail :: Op xs [a] -> Op xs [a]
  Map :: Op '[a] b -> Op xs [a] -> Op xs [b]
  Filter :: Op '[a] Bool -> Op xs [a] -> Op xs [a]

type AST :: [Type] -> Type -> Type
type AST xs a = Op xs a