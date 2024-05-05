{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Types (Every, KnownNatList (..), Count) where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

type If :: Bool -> a -> a -> a
type family If b x y where
  If 'True x y = x
  If 'False x y = y

type (==) :: a -> b -> Bool
type family a == b where
  a == b = a == b

type (>?) :: Nat -> Nat -> Ordering
type family a >? b where
  a >? b = CmpNat a b

type (>) :: Nat -> Nat -> Bool
type family a > b where
  a > b = a >? b == 'GT

type Every :: (a -> Constraint) -> [a] -> Constraint
type family Every c as where
  Every c '[] = ()
  Every c (a ': as) = (c a, Every c as)

class KnownNatList (as :: [Nat]) where
  natListVal :: Proxy as -> [Integer]

instance KnownNatList '[] where
  natListVal _ = []

instance (KnownNat a, KnownNatList as) => KnownNatList (a ': as) where
  natListVal _ = natVal (Proxy @a) : natListVal (Proxy @as)

type Count :: [a] -> Nat
type family Count as where
  Count '[] = 0
  Count (a ': as) = 1 + Count as
