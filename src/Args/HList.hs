{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Args.HList (module Args.HList) where

import Data.Kind (Type)

type HList :: [Type] -> Type
data HList xs where
  Nil :: HList '[]
  (:.) :: x -> HList xs -> HList (x ': xs)

infixr 5 :.

type Args :: [Type] -> Type
type Args xs = HList xs