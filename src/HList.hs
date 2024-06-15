{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module HList where

import Data.Kind (Constraint, Type)
import GHC.TypeNats
import MyNat

type HList :: [Type] -> Type
data HList xs where
  Nil :: HList '[]
  (:.) :: x -> HList xs -> HList (x ': xs)

infixr 5 :.

data SomeType where
  SomeVal :: a -> SomeType

type Pickable :: [Type] -> Nat -> Constraint
class (KnownNat n, n < Len xs) => Pickable xs n where
  (!.) :: HList xs -> SNat n -> xs ! n

instance (0 < Len (x : xs)) => Pickable (x : xs) 0 where
  (x :. _) !. _ = x

instance
  {-# OVERLAPPABLE #-}
  ( KnownNat n,
    Pickable xs m,
    n ~ (m + 1),
    (xs ! m) ~ ((x : xs) ! n),
    n < Len (x : xs)
  ) =>
  Pickable (x : xs) n
  where
  (_ :. ys) !. _ = (!.) @xs @m ys (SNat @m)

hhead :: HList (x : xs) -> x
hhead (x :. _) = x

htail :: HList (x : xs) -> HList xs
htail (_ :. xs) = xs

-- f :: HList (x : xs) -> a
-- f xs = hhead $ htail xs

-- TODO: implementar funcao At
-- (!.) :: forall n x xs . (KnownNat n, n < Len (x : xs)) =>
--   HList (x : xs) -> SNat n -> (x : xs) ! n
-- (x :. xs) !. sn = case natVal sn of
--   0 -> x
--   _ -> xs !. natSing @(n - 1)

-- (_ :. xs) !. n = xs !. (n - 1)