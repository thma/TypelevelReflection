{-# LANGUAGE TypeOperators #-} -- :*: and :+:
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( someFunc
    ) where

import GHC.Generics

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

data Foo a b c =
    F0
  | F1 a
  | F2 b c
  deriving (Generic)