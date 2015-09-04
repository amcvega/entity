{-# LANGUAGE GADTs, FlexibleContexts #-}
module Entity.Common where

import Data.Convertible

import Entity.Query
import Entity.Definition
import Entity.Core
import Entity.StoreVal


data CommonI a where
    FindRecord :: (Storable a) => Key a -> CommonI (Maybe (Entity a))
    FindRecordsByIndex :: (Storable a, Convertible val StoreVal)
                          => StoreField a val -> val -> CommonI [Entity a]
    CreateRecord :: (Storable a) => a -> CommonI (Key a)
    UpdateRecord :: (Storable a) => Entity a -> Entity a -> CommonI (Entity a)
    DeleteRecord :: (Storable a) => Entity a -> CommonI ()
    FindRecordsByIndices :: (Storable a)
                            => [Filter a] -> CommonI [Entity a]
    FindInterUnion :: (Storable a)
                      => [Filter a] -> [Filter a] -> CommonI [Entity a]
    FilterQuery :: (Storable a) => SimpleQuery a -> CommonI (QueryResult a)
    FindRecordByUnique :: (Storable a, Convertible val StoreVal)
                          => StoreField a val
                          -> val -> CommonI (Maybe (Entity a))
