{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module MyNat where

import Data.Kind (Constraint, Type)
import Data.Void
import GHC.TypeNats

type Len :: [Type] -> Nat
type family Len a where
  Len '[] = 0
  Len (_ : xs) = 1 + Len xs

type (<?) :: Nat -> Nat -> Bool
type family n <? m where
  n <? 0 = 'False
  0 <? m = 'True
  n <? m = (n - 1) <? (m - 1)

type (<) :: Nat -> Nat -> Constraint
type family n < m where
  n < m = n <? m ~ 'True

-- Isso aqui eh um pouco incomodo
type (!) :: [Type] -> Nat -> Type
type family xs ! n where
  '[] ! _ = Void
  (x ': _) ! 0 = x
  (_ ': xs) ! n = xs ! (n - 1)

type NonEmpty :: [Type] -> Constraint
type family NonEmpty xs where
  NonEmpty xs = 0 < Len xs

type IsHead :: Nat -> Bool
type family IsHead n where
  IsHead 0 = True
  IsHead _ = False