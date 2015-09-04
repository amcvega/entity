{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, TypeFamilies
           , GADTs, DefaultSignatures, ExistentialQuantification
           , GeneralizedNewtypeDeriving #-}

module Entity.Core
       ( Storable(..)
       , MetaStore(..)
       , Field(..)
       , FieldDef(..)
       , FieldList(..)
       , Sorted(..)
       -- , storeLookup'
       , storeLookup
       , provide
       , fieldStore
       , fieldOf
       , sortedIndexVals
       -- , redisToEntity
       , listNewFields
       , listChanges
       , toEntity
       )
       where

import Control.Arrow ((&&&))

import Data.Monoid
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


toEntity :: (Storable a) => Int -> a -> Entity a
toEntity kid record = Entity (Key kid) record


class MetaStore a where
    storeName :: a -> String
    default storeName :: Typeable a => a -> String
    storeName = show . typeOf

instance MetaStore a => MetaStore (Entity a) where
    storeName ent = storeName (eVal ent)


newtype FieldList a = FieldList { fieldList :: [(Field a, StoreVal)] }
                      deriving (Monoid)


class (Typeable a, MetaStore a) => Storable a where
    data StoreField a typ



    fieldDef :: StoreField a typ -> FieldDef
    fieldDef = FieldDef . fst . fieldPairs

    fieldAttr :: StoreField a typ -> (a -> typ)
    fieldAttr = snd . fieldPairs

    fieldPairs :: StoreField a typ -> (String, (a -> typ))
    fieldPairs = (fieldName . fieldDef) &&& fieldAttr

    assemble :: Map.Map String StoreVal -> Maybe a

    storeFields :: a -> [Field a]
    storeFields _ = []

    uniques :: a -> [Field a]
    uniques _ = []

    indices :: a -> [Field a]
    indices _ = []

    sorted :: a -> [Sorted a]
    sorted _ = []

    -- | List of unique indices
    uniqueVals :: a -> FieldList a
    uniqueVals f = FieldList $ map genvals $ uniques f
        where genvals fld@(Field name) = (fld, toStore $ fieldAttr name f)
        -- where genvals (Field name) = (fieldOf name, toStore $ fieldAttr name f)

    -- | List of regular indices
    indexVals :: a -> FieldList a
    indexVals f = FieldList $ map genvals $ indices f
        where genvals fld@(Field name) = (fld, toStore $ fieldAttr name f)

    -- | Convert a record into a list of storevals for the db
    toList :: a -> FieldList a
    toList f = FieldList $ map genvals $ storeFields f
        where genvals fld@(Field name) = (fld, toStore $ fieldAttr name f)


    listToStorable :: [(String, StoreVal)] -> Either String a
    listToStorable xs = case assemble (Map.fromList xs) of
        Just f -> Right f
        Nothing -> Left $ "Missing Fields: " ++ show xs


    -- | Try to convert a list of storevals into a record. can fail.
    -- listToStorable :: [(String, StoreVal)] -> Either String a
    -- listToStorable xs = case assemble xs of
    --     Just f -> Right f
    --     Nothing -> Left "Missing Fields"


sortedIndexVals :: Storable a => a -> [(String, StoreVal)]
sortedIndexVals f = map genvals $ sorted f
    where genvals (Sorted name) = (fieldOf name, toStore $ fieldAttr name f)


-- | Trick to return the type of the  store of a storefield
fieldStore :: Storable a => StoreField a b -> a
fieldStore = undefined


-- | Wraper for StoreFields to allow them to be put into a list.
-- data Field a where
--     Field :: (Eq b, Show b, Convertible b StoreVal)
--              => StoreField a b -> Field a
--     KeyField :: Field a
data Field a = forall b. (Eq b, Show b, Convertible b StoreVal)
               => Field (StoreField a b)
             | KeyField



data Sorted a =
    forall b. (Eq b, Show b, Convertible b StoreVal, Convertible b Double)
    => Sorted (StoreField a b)

-- data Sorted a where
--     Sorted :: (Eq b
--               , Show b, Convertible b StoreVal
--               , Convertible b Double)p
--              => StoreField a b -> Sorted a


newtype FieldDef = FieldDef { fieldName :: String}
                 deriving (Ord, Eq)


-- | convenience function to translate StoreField to string
fieldOf :: (Storable a) => StoreField a b -> String
fieldOf = fieldName . fieldDef
{-# INLINE fieldOf #-}

-- |Use a String to lookup values from "db"
-- storeLookup :: (Eq a, Convertible StoreVal b)
--                => a
--                -> [(a, StoreVal)] -> Maybe b
-- storeLookup k xs = fromStore `fmap` lookup k xs


-- |Use a StoreField to lookup values from "db"
storeLookup :: (Convertible StoreVal b, Storable a)
                => StoreField a b -> Map.Map String StoreVal -> Maybe b
storeLookup k xs = fromStore `fmap` Map.lookup (fieldOf k) xs


-- | Wrapper around storeLookup' functionality.  Provides default
--   value if lookup for a particular field fails (doesn't exist in db)
provide :: (Convertible StoreVal b
           -- , Convertible ByteString b
           , Storable a, Typeable b)
           => b -> StoreField a b -> Map.Map String StoreVal -> Maybe b
provide def sf xs =
    maybe (Just def) Just (fromStore `fmap` Map.lookup (fieldOf sf) xs)


listNewFields :: Storable a => a -> [(String, Text)]
listNewFields record = foldr appendField [] (storeFields record)
    where
        appendField (Field field) acc =
            let val = fieldAttr field record in
            (fieldOf field, T.pack (show val)) : acc


listChanges :: (Storable a) => a -> a -> [(String, Text, Text)]
listChanges old new = foldr appendDifference [] (storeFields old)
    where
        appendDifference (Field field) acc =
            let oldval = fieldAttr field old
                newval = fieldAttr field new
            in if oldval == newval
               then acc
               else
                   (fieldOf field, T.pack $ show oldval, T.pack $ show newval)
                   : acc
