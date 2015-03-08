{-# LANGUAGE DeriveGeneric #-}
module Api.Types where

import GHC.Generics
import Data.ByteString (ByteString)
import Data.Binary
import Data.Map

type VerbList  = [String]
type LemmeMap  = Map String [String]
type SampleMap = Map String [ByteString]

data Database = Database
    { verbList  :: VerbList
    , lemmeMap  :: LemmeMap
    , sampleMap :: SampleMap
    } deriving (Show, Generic)

instance Binary Database

