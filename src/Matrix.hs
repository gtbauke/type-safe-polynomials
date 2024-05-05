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

module Matrix where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

type Matrix :: Nat -> Nat -> Type -> Type
data Matrix rows cols a where
  Matrix :: (KnownNat rows, KnownNat cols, Num a) => [[a]] -> Matrix rows cols a

rows :: forall rows cols a. (KnownNat rows) => Matrix rows cols a -> Int
rows _ = fromIntegral $ natVal (Proxy @rows)

cols :: forall rows cols a. (KnownNat cols) => Matrix rows cols a -> Int
cols _ = fromIntegral $ natVal (Proxy @cols)

instance (Show a) => Show (Matrix rows cols a) where
  show (Matrix rows) = matrix
    where
      rowStrings = map showRow rows
      showRow row = "[" <> foldr1 (\e acc -> e <> ", " <> acc) (map show row) <> "]"
      matrix = foldr1 (\r acc -> r <> "\n" <> acc) rowStrings

add :: forall rows cols a. (KnownNat rows, KnownNat cols, Num a) => Matrix rows cols a -> Matrix rows cols a -> Matrix rows cols a
add (Matrix as) (Matrix bs) = Matrix $ zipWith (zipWith (+)) as bs

scale :: forall rows cols a. (KnownNat rows, KnownNat cols, Num a) => a -> Matrix rows cols a -> Matrix rows cols a
scale s (Matrix as) = Matrix $ map (map (s *)) as

transpose :: forall rows cols a. (KnownNat rows, KnownNat cols, Num a) => Matrix rows cols a -> Matrix cols rows a
transpose (Matrix as) = Matrix $ foldr (zipWith (:)) (repeat []) as

mult :: forall rows cols1 cols2 a. (KnownNat rows, KnownNat cols1, KnownNat cols2, Num a) => Matrix rows cols1 a -> Matrix cols1 cols2 a -> Matrix rows cols2 a
mult (Matrix as) bs = Matrix rows
  where
    rows = [[sum $ zipWith (*) a b | b <- bs'] | a <- as]
    (Matrix bs') = transpose bs

m1 = Matrix [[1, 2, 3], [4, 5, 6]] :: Matrix 2 3 Int

m2 = Matrix [[1, 2], [3, 4], [5, 6]] :: Matrix 3 2 Int

m3 = Matrix [[1, 2], [3, 4]] :: Matrix 2 2 Int

m4 = Matrix [[1, 2], [3, 4], [5, 6]] :: Matrix 3 2 Int

m5 = Matrix [[1, 2, 3], [4, 5, 6]] :: Matrix 2 3 Int

m6 = Matrix [[1, 2], [3, 4], [5, 6]] :: Matrix 3 2 Int

-- TODO: manipulation context that can be used to keep track of the operations done on a matrix
