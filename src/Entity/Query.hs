{-# LANGUAGE GADTs, FlexibleContexts #-}
module Entity.Query where

import Data.Convertible
import Entity.StoreVal
import Entity.Definition
import Entity.Core


data Filter a where
    Filter :: (Storable a, Convertible b StoreVal)
                 => StoreField a b -> b -> Filter a
    RangeFilter :: (Storable a, Convertible b StoreVal, Convertible b Double)
                   => StoreField a b -> b -> b -> Filter a
    GreaterFilter :: (Storable a, Convertible b StoreVal, Convertible b Double)
                     => StoreField a b -> b -> Filter a
    LesserFilter :: (Storable a, Convertible b StoreVal, Convertible b Double)
                     => StoreField a b -> b -> Filter a
    IncludeFilter :: (Storable a, Convertible b StoreVal)
                     => StoreField a b -> [b] -> Filter a
    NullFilter :: (Storable a, Convertible b StoreVal)
                 => StoreField a b -> Filter a
    KeyFilter :: [Key a] -> Filter a


-- (.=) :: (Storable a, Convertible b StoreVal)
--         => StoreField a b -> b -> Filter a
-- x .= y = Filter x y


(%==%) :: (Storable a, Convertible b StoreVal)
        => StoreField a b -> b -> Filter a
x %==% y = Filter x y


(%=) :: (Storable a, Convertible b StoreVal)
        => StoreField a b -> b -> Filter a
x %= y = Filter x y


(.?) :: (Storable a, Convertible b StoreVal)
        => StoreField a b -> [b] -> Filter a
x .? y = IncludeFilter x y


(%=?) :: (Storable a, Convertible b StoreVal)
        => StoreField a b -> [b] -> Filter a
x %=? y = IncludeFilter x y


data Direction = Asc | Desc


type Offset = Integer
type LimitQty = Integer
data Limit = Limit Offset LimitQty
           | NoLimit

data Sort a where
    Sort :: (Storable a, Convertible b StoreVal)
            => Direction -> StoreField a b -> Sort a


data ResultType = ResultAll | ResultKeys | ResultCount

data QueryResult a = Count Int
                   | Keys { ekeys :: [Key a]}
                   | Entries { entries :: [Entity a] }

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

mergeQuery :: SimpleQuery a -> SimpleQuery a -> SimpleQuery a
mergeQuery old new = old { qInt = (qInt old) ++ (qInt new)
                         , qUnion = (qUnion old) ++ (qUnion new)
                         , qResult = qResult new
                         , qNot = qNot old ++ qNot new
                         , qSort = qSort old ++ qSort new
                         , qLimit = qLimit new
                         }

querySubject :: SimpleQuery a -> a
querySubject = undefined

queryStore :: MetaStore a => SimpleQuery a -> String
queryStore = storeName .  querySubject
