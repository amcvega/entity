{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, TypeFamilies
           , RankNTypes, GADTs, DefaultSignatures #-}

module Entity.Core
       ( Storeable(..)
       , MetaStore(..)
       , Field(..)
       , FieldDef(..)
       , storeLookup'
       , storeLookup
       , provide
       , fieldStore
       , fieldOf
       , sortedIndexVals
       , redisToEntity
       , listNewFields
       , listChanges
       , toEntity
       )
       where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, TimeZone, Day)
import Data.Data (Typeable, typeOf)
-- |Redis Imports
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Entity.StoreVal
import Entity.Definition
import Data.Convertible
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

-- class Redisable a where
--     toRedis :: a -> [(ByteString, ByteString)]


-- instance (Storeable a) => Redisable (Entity a) where
--     toRedis x = getVals
--         where
--             getVals = map bsPairs $ toList $ eVal x
--             bsPairs (k,v) = (B.pack k, fromStore v)


toEntity :: (Storeable a) => Int -> a -> Entity a
toEntity kid record = Entity (Key kid) record

redisToEntity :: Storeable a
              => Key a
              -> [(ByteString, ByteString)]
              -> Either String (Entity a)
redisToEntity kid vals = Entity kid `fmap` (listToStoreable $ map unbsPairs vals)
    where
        unbsPairs (k,v) = (B.unpack k, toStore v)

class MetaStore a where
    storeName :: a -> String
    default storeName :: Typeable a => a -> String
    storeName = show . typeOf

instance MetaStore a => MetaStore (Entity a) where
    storeName ent = storeName (eVal ent)


class (Typeable a, MetaStore a) => Storeable a where
    data StoreField a typ

    fieldDef :: StoreField a typ -> FieldDef
    fieldAttr :: StoreField a typ -> (a -> typ)

    assemble :: Map.Map String StoreVal -> Maybe a
    -- assemble :: [(String, StoreVal)] -> Maybe a

    storeFields :: a -> [Field a]
    storeFields _ = []

    uniques :: a -> [Field a]
    uniques _ = []

    indices :: a -> [Field a]
    indices _ = []

    sorted :: a -> [Field a]
    sorted _ = []

    -- | List of unique indices
    uniqueVals :: a -> [(String, StoreVal)]
    uniqueVals f = map genvals $ uniques f
        where genvals (Field name) = (fieldOf name, toStore $ fieldAttr name f)

    -- | List of regular indices
    indexVals :: a -> [(String, StoreVal)]
    indexVals f = map genvals $ indices f
        where genvals (Field name) = (fieldOf name, toStore $ fieldAttr name f)

    -- | Convert a record into a list of storevals for the db
    toList :: a -> [(String, StoreVal)]
    toList f = map genvals $ storeFields f
        where genvals (Field name) = (fieldOf name, toStore $ fieldAttr name f)


    listToStoreable :: [(String, StoreVal)] -> Either String a
    listToStoreable xs = case assemble (Map.fromList xs) of
        Just f -> Right f
        Nothing -> Left $ "Missing Fields: " ++ show xs

    -- | Try to convert a list of storevals into a record. can fail.
    -- listToStoreable :: [(String, StoreVal)] -> Either String a
    -- listToStoreable xs = case assemble xs of
    --     Just f -> Right f
    --     Nothing -> Left "Missing Fields"


sortedIndexVals :: Storeable a => a -> [(String, StoreVal)]
sortedIndexVals f = map genvals $ sorted f
    where genvals (Field name) = (fieldOf name, toStore $ fieldAttr name f)


-- | Trick to return the type of the  store of a storefield
fieldStore :: Storeable a => StoreField a b -> a
fieldStore = undefined

-- | Wraper for StoreFields to allow them to be put into a list.
data Field a where
    Field :: (Eq b, Convertible b Text, Show b, Convertible b StoreVal)
             => StoreField a b -> Field a

data Filter a where
    Filter :: (Storeable a, Convertible b StoreVal)
                 => StoreField a b -> b -> Filter a



newtype FieldDef = FieldDef { fieldName :: String}

-- | convenience function to translate StoreField to string
fieldOf :: (Storeable a) => StoreField a b -> String
fieldOf = fieldName . fieldDef
{-# INLINE fieldOf #-}

-- |Use a String to lookup values from "db"
storeLookup :: (Eq a, Convertible StoreVal b)
               => a
               -> [(a, StoreVal)] -> Maybe b
storeLookup k xs = fromStore `fmap` lookup k xs


-- |Use a StoreField to lookup values from "db"
storeLookup' :: (Convertible StoreVal b, Storeable a)
                => StoreField a typ -> Map.Map String StoreVal -> Maybe b
storeLookup' k xs = fromStore `fmap` Map.lookup (fieldOf k) xs


-- | Wrapper around storeLookup' functionality.  Provides default
--   value if lookup for a particular field fails (doesn't exist in db)
provide :: (Convertible StoreVal b
           , Convertible ByteString b
           , Storeable a, Typeable b)
           => b -> StoreField a b -> Map.Map String StoreVal -> Maybe b
provide def sf xs =
    maybe (Just def) Just (fromStore `fmap` Map.lookup (fieldOf sf) xs)


listNewFields :: Storeable a => a -> [(String, Text)]
listNewFields record = foldr appendField [] (storeFields record)
    where
        appendField (Field field) acc =
            let val = fieldAttr field record in
            (fieldOf field, T.pack (show val)) : acc


listChanges :: (Storeable a) => a -> a -> [(String, Text, Text)]
listChanges old new = foldr appendDifference [] (storeFields old)
    where
        appendDifference (Field field) acc =
            let oldval = fieldAttr field old
                newval = fieldAttr field new
            in if oldval == newval
               then acc
               else (fieldOf field, convert oldval, convert newval) : acc
