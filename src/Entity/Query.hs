{-# LANGUAGE GADTs, FlexibleContexts #-}
module Entity.Query where

import Data.Convertible
import Entity.StoreVal
import Entity.Definition
import Entity.Core


data Filter a where
    Filter :: (Storeable a, Convertible b StoreVal)
                 => StoreField a b -> b -> Filter a
    RangeFilter :: (Storeable a, Convertible b StoreVal, Convertible b Double)
                   => StoreField a b -> b -> b -> Filter a


(.=) :: (Storeable a, Convertible b StoreVal)
        => StoreField a b -> b -> Filter a
x .= y = Filter x y


data Direction = Asc | Desc

data Limit = Limit Integer Integer
           | NoLimit

data Sort a where
    Sort :: (Storeable a, Convertible b StoreVal)
            => Direction -> (StoreField a b) -> Sort a


data ResultType = ResultAll | ResultCount

data QueryResult a = Count Int
                   | Entries [Entity a]

data SimpleQuery a = SimpleQuery
             { qResult    :: ResultType
             , qInt       :: [Filter a]
             , qUnion     :: [Filter a]
             , qNot       :: [Filter a]
             , qSort      :: [Sort a]
             , qLimit     :: Limit
             }

defaultQuery :: SimpleQuery a
defaultQuery = SimpleQuery ResultAll [] [] [] [] NoLimit

query :: SimpleQuery a
query = defaultQuery

querySubject :: SimpleQuery a -> a
querySubject = undefined

queryStore :: MetaStore a => SimpleQuery a -> String
queryStore = storeName .  querySubject
