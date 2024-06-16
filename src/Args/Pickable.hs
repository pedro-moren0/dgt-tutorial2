{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Args.Pickable (module Args.Pickable) where

import Args.FamilyUtils
import Args.HList
import Data.Kind (Constraint, Type)
import GHC.TypeNats (KnownNat (natSing), Nat, SNat, type (+))

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
  (_ :. ys) !. _ = (!.) @xs @m ys (natSing @m)