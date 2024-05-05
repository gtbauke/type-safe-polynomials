{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Poly where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

type Poly :: a -> Nat -> Type
data Poly a degree where
  Poly :: (KnownNat degree, Num a) => [a] -> Poly a degree

instance (Show a) => Show (Poly a degree) where
  show (Poly coefficients) = poly
    where
      degree = natVal (Proxy @degree)
      degrees = [degree, degree - 1 .. 0]
      terms = zipWith (\c d -> show c <> if d == 0 then "" else "x^" <> show d) coefficients degrees
      poly = foldr1 (\t acc -> t <> " + " <> acc) terms

degree :: forall a degree. (KnownNat degree) => Poly a degree -> Int
degree _ = fromIntegral $ natVal (Proxy @degree)

add :: forall a degree. (KnownNat degree, Num a) => Poly a degree -> Poly a degree -> Poly a degree
add (Poly as) (Poly bs) = Poly $ zipWith (+) as bs

scale :: forall a degree. (KnownNat degree, Num a) => a -> Poly a degree -> Poly a degree
scale s (Poly as) = Poly $ map (s *) as

mult :: forall a degree1 degree2. (KnownNat (degree1 + degree2), Num a) => Poly a degree1 -> Poly a degree2 -> Poly a (degree1 + degree2)
mult (Poly as) (Poly bs) = Poly coefficients
  where
    terms = [[a * b | a <- as] | b <- bs]
    coefficients = foldr1 (zipWith (+)) terms
