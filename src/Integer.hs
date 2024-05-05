{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Integer (SInt (..), type (+:), IntKind (..), KnownInt (..), type (-:), KnownInt' (..)) where

import Data.Kind
import Data.Proxy
import Data.Type.Bool (If)
import Data.Type.Ord (type (>?))
import GHC.TypeLits

data IntKind = KPositive | KNegative | KZero deriving (Show, Eq)

type SInt :: IntKind -> Nat -> Type
data SInt k n where
  Positive :: SInt KPositive n
  Negative :: SInt KNegative n
  Zero :: SInt KZero 0

class (KnownNat n) => KnownInt k n where
  intVal :: Proxy k -> Proxy n -> Integer

instance (KnownNat n) => KnownInt KPositive n where
  intVal _ _ = natVal @n Proxy

instance (KnownNat n) => KnownInt KNegative n where
  intVal _ _ = -natVal @n Proxy

instance (KnownNat n) => KnownInt KZero n where
  intVal _ _ = 0

instance (KnownNat n) => Show (SInt k n) where
  show Positive = show (natVal @n Proxy)
  show Negative = "-" <> show (natVal @n Proxy)
  show Zero = "0"

class KnownInt' i where
  intVal' :: Proxy i -> Integer

instance (KnownInt k n) => KnownInt' (SInt k n) where
  intVal' _ = intVal (Proxy @k) (Proxy @n)

type family Negate a where
  Negate (SInt KPositive n) = SInt KNegative n
  Negate (SInt KNegative n) = SInt KPositive n
  Negate (SInt KZero 0) = SInt KZero 0

type family (+:) a b where
  (SInt KPositive m) +: (SInt KPositive n) = SInt KPositive (m + n)
  (SInt KPositive m) +: (SInt KNegative n) = If (m >? n) (SInt KPositive (m - n)) (SInt KNegative (n - m))
  (SInt KNegative m) +: (SInt KPositive n) = If (m >? n) (SInt KNegative (m - n)) (SInt KPositive (n - m))
  (SInt KNegative m) +: (SInt KNegative n) = SInt KNegative (m + n)
  (SInt KZero 0) +: (SInt k n) = SInt k n
  (SInt k n) +: (SInt KZero 0) = SInt k n

type family (-:) a b where
  a -: b = a +: Negate b

class KnownSign k where
  signVal :: Proxy k -> IntKind

instance KnownSign KPositive where
  signVal _ = KPositive

instance KnownSign KNegative where
  signVal _ = KNegative

instance KnownSign KZero where
  signVal _ = KZero

isOppositeSign :: forall k1 k2. (KnownSign k1, KnownSign k2) => Proxy k1 -> Proxy k2 -> Bool
isOppositeSign p1 p2 = signVal p1 /= signVal p2

-- (+:) :: forall a b k1 k2. (KnownSign k1, KnownSign k2, KnownNat a, KnownNat b) => SInt k1 a -> SInt k2 b ->
-- (+:) _ _
--   | isOppositeSign (Proxy @k1) (Proxy @k2) && n1 == n2 = Zero
--   | s1 == KPositive = Positive
--   | otherwise = Negative
--   where
--       s1 = signVal $ Proxy @k1
--       s2 = signVal $ Proxy @k2
--       n1 = natVal $ Proxy @a
--       n2 = natVal $ Proxy @b
