{-# LANGUAGE GADTs, FlexibleContexts #-}
module Entity.Common where

import Data.Convertible

import Entity.Query
import Entity.Definition
import Entity.Core
import Entity.StoreVal


data CommonI a where
    FindRecord :: (Storeable a) => Key a -> CommonI (Maybe (Entity a))
    FindRecordsByIndex :: (Storeable a, Convertible val StoreVal)
                          => StoreField a val -> val -> CommonI [Entity a]
    CreateRecord :: (Storeable a) => a -> CommonI (Key a)
    UpdateRecord :: (Storeable a) => Entity a -> Entity a -> CommonI (Entity a)
    DeleteRecord :: (Storeable a) => Entity a -> CommonI ()
    FindRecordsByIndices :: (Storeable a)
                            => [Filter a] -> CommonI [Entity a]
    FindInterUnion :: (Storeable a)
                      => [Filter a] -> [Filter a] -> CommonI [Entity a]
    FilterQuery :: (Storeable a) => SimpleQuery a -> CommonI (QueryResult a)
    FindRecordByUnique :: (Storeable a, Convertible val StoreVal)
                          => StoreField a val
                          -> val -> CommonI (Maybe (Entity a))
