{-# LANGUAGE DeriveDataTypeable #-}
module Entity.Definition where

import Data.Data

newtype Key a = Key { unKey :: Int }
              deriving (Eq, Ord, Data, Typeable)

instance Show (Key a) where
    show = show . unKey


instance Read (Key a) where
    readsPrec d r = do
        (v, r') <- readsPrec d r
        return (Key v, r')


data Entity entity = Entity
    { eKey :: Key entity
    , eVal :: entity
    } deriving (Show,  Read, Data, Typeable)


instance Eq (Entity a) where
    x == y = eKey x == eKey y

instance Ord (Entity a) where
    compare (Entity x _) (Entity y _) = compare x y
