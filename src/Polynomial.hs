{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Polynomial (Polynomial (..), PrettyShow (..), Degree, canBaskharaSolve, type (.+.), type (.-.)) where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Types (Count, Every, KnownNatList (..))

class PrettyShow a where
  prettyShow :: a -> String

type Polynomial :: [Nat] -> Type
data Polynomial as where
  Polynomial :: (Every KnownNat as) => Polynomial as

instance (KnownNatList as, KnownNat (Count as - 1)) => Show (Polynomial as) where
  show Polynomial = "g: " <> show (natVal (Proxy @(Count as - 1))) <> " " <> show (natListVal (Proxy @as))

instance (KnownNatList as, KnownNat (Count as - 1)) => PrettyShow (Polynomial as) where
  prettyShow Polynomial = poly
    where
      coefficients = reverse $ natListVal (Proxy @as)
      degree = natVal (Proxy @(Count as - 1))

      degrees = [degree, degree - 1 .. 0]
      terms = zipWith (\c d -> show c <> if d == 0 then "" else "x^" <> show d) coefficients degrees
      poly = foldr1 (\t acc -> t <> " + " <> acc) terms

type Degree :: as -> Nat
type family Degree p where
  Degree (Polynomial as) = Count as - 1

type family AddCoefficients as bs where
  AddCoefficients '[] '[] = '[]
  AddCoefficients (a : as) '[] = a : AddCoefficients as '[]
  AddCoefficients '[] (b : bs) = b : AddCoefficients '[] bs
  AddCoefficients (a : as) (b : bs) = a + b : AddCoefficients as bs

type family SubCoefficients as bs where
  SubCoefficients '[] '[] = '[]
  SubCoefficients (a : as) '[] = a : SubCoefficients as '[]
  SubCoefficients '[] (b : bs) = 0 - b : SubCoefficients '[] bs
  SubCoefficients (a : as) (b : bs) = a - b : SubCoefficients as bs

type family (.+.) as bs where
  (.+.) (Polynomial as) (Polynomial bs) = Polynomial (AddCoefficients as bs)

type family (.-.) as bs where
  (.-.) (Polynomial as) (Polynomial bs) = Polynomial (SubCoefficients as bs)

canBaskharaSolve :: forall p. (KnownNat (Degree p)) => p -> Bool
canBaskharaSolve _ = natVal (Proxy @(Degree p)) <= 2
