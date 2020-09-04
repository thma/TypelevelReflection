{-# LANGUAGE TypeOperators #-}     -- :*: and :+:
{-# LANGUAGE DeriveGeneric #-}     -- have Generic in the deriving clause
{-# LANGUAGE DefaultSignatures #-} -- provide default keyword as in default eq
{-# LANGUAGE FlexibleContexts #-}  -- allow Non type-variable argument in the constraint: GEq (Rep a)
{-# LANGUAGE DeriveAnyClass #-}    -- automatic deriving for user defined classes
{-# LANGUAGE StandaloneDeriving #-} --
module Lib where

import GHC.Generics

class MyEq a where
  eq :: a -> a -> Bool
  default eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
  eq a b = geq (from a) (from b)

class GEq a where
  geq :: a x -> a x -> Bool

instance GEq U1 where
  geq U1 U1 = True

instance GEq V1 where
  geq _ _ = True

instance Eq a => GEq (K1 _l a) where
  geq (K1 a) (K1 b) = a == b

instance (GEq a, GEq b) => GEq (a :+: b) where
  geq (L1 a1) (L1 a2) = geq a1 a2
  geq (R1 b1) (R1 b2) = geq b1 b2
  geq _ _             = False

instance (GEq a, GEq b) => GEq (a :*: b) where
  geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2

instance GEq a => GEq (M1 _x _y a) where
  geq (M1 a1) (M1 a2) = geq a1 a2

data Foo a b c =
    F0
  | F1 a
  | F2 b c
  deriving (Generic, MyEq)

--genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
--genericEq a b = geq (from a) (from b)
--
--instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
--  (==) = genericEq

