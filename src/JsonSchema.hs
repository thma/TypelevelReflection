{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module JsonSchema where

import Control.Monad.Writer
import Data.Aeson (Value (..), (.=), object)
import Data.Kind (Type)
import Data.Text (Text, pack)
import Data.Typeable
import Data.Vector (fromList)
import GHC.Generics
import GHC.TypeLits
import qualified GHC.TypeLits as Err

data Person = Person
  { name :: String
  , age :: Int
  , phone :: Maybe String
  , permissions :: [Bool]
  }
  deriving (Generic)
  
class GSchema (a :: Type -> Type) where
  gschema :: Writer [Text] Value

-- helper functions
mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object $ a <> b

emitRequired :: forall nm . KnownSymbol nm => Writer [Text] ()
emitRequired = tell . pure . pack . symbolVal $ Proxy @nm

type family ToJSONType (a :: Type) :: Symbol where
  ToJSONType Int     = "integer"
  ToJSONType Integer = "integer"
  ToJSONType Float   = "number"
  ToJSONType Double  = "number"
  ToJSONType String  = "string"
  ToJSONType Bool    = "boolean"
  ToJSONType [a]     = "array"
  ToJSONType a       = TypeName a