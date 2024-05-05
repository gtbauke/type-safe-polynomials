{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Floats (SFloat (..), KnownFloat (..)) where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

type SFloat :: Nat -> Nat -> Type
data SFloat m n where
  Float :: (KnownNat m, KnownNat n) => SFloat m n

class KnownFloat (m :: Nat) (n :: Nat) where
  floatVal :: Proxy m -> Proxy n -> Double

instance (KnownNat m, KnownNat n) => KnownFloat m n where
  floatVal _ _ = fromIntegral (natVal (Proxy @m)) / fromIntegral (natVal (Proxy @n))
