{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Args.FamilyUtils (module Args.FamilyUtils) where

import Data.Kind (Constraint, Type)
import Data.Void (Void)
import GHC.TypeNats (Nat, type (+), type (-))

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

type (!) :: [Type] -> Nat -> Type
type family xs ! n where
  '[] ! _ = Void
  (x ': _) ! 0 = x
  (_ ': xs) ! n = xs ! (n - 1)