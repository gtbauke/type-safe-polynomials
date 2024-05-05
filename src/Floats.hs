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

module Floats (MMC, MDC, type (+.), SFloat (..), type (-.), type (/.), type (*.)) where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

class FractionalShow a where
  fractionalShow :: a -> String

type SFloat :: Nat -> Nat -> Type
data SFloat m n where
  Fractional :: SFloat m n
  Integer :: SFloat m 1

class (KnownNat m, KnownNat n) => KnownFloat m n where
  doubleVal :: Proxy m -> Proxy n -> Double

instance (KnownNat m, KnownNat n) => KnownFloat m n where
  doubleVal _ _ = fromIntegral (natVal @m Proxy) / fromIntegral (natVal @n Proxy)

instance (KnownNat m, KnownNat n) => FractionalShow (SFloat m n) where
  fractionalShow Fractional = show (natVal @m Proxy) <> "/" <> show (natVal @n Proxy)
  fractionalShow Integer = show (natVal @m Proxy)

instance (FractionalShow (SFloat m n)) => Show (SFloat m n) where
  show = fractionalShow

type MDC :: Nat -> Nat -> Nat
type family MDC a b where
  MDC a 0 = a
  MDC a b = MDC b (GHC.TypeLits.Mod a b)

type MMC :: Nat -> Nat -> Nat
type family MMC a b where
  MMC a b = GHC.TypeLits.Div (a GHC.TypeLits.* b) (MDC a b)

type family (+.) a b where
  (SFloat m1 n1) +. (SFloat m2 n2) = SFloat (m1 GHC.TypeLits.* n2 + m2 GHC.TypeLits.* n1) (n1 GHC.TypeLits.* n2)

type family (-.) a b where
  (SFloat m1 n1) -. (SFloat m2 n2) = SFloat (m1 GHC.TypeLits.* n2 - m2 GHC.TypeLits.* n1) (n1 GHC.TypeLits.* n2)

type family (*.) a b where
  (SFloat m1 n1) *. (SFloat m2 n2) = SFloat (m1 GHC.TypeLits.* m2) (n1 GHC.TypeLits.* n2)

type family (/.) a b where
  (SFloat m1 n1) /. (SFloat m2 n2) = SFloat (m1 GHC.TypeLits.* n2) (n1 GHC.TypeLits.* m2)
