{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module Entity.Convertible where

import Data.Data (Typeable)
import Safe (readMay)
import Data.Time -- (UTCTime, Day, TimeZone, toModifiedJulianDay)
import Data.Time.Clock.POSIX
import Data.Convertible
import Data.Text.Encoding ( decodeUtf8)

import Data.ByteString.Char8 as C
import qualified Data.Text as T

import Entity.Definition

instance Convertible Int (Key a) where
    safeConvert = return . Key

instance Convertible (Key a) T.Text where
    safeConvert = return . T.pack . show . unKey

instance Convertible UTCTime T.Text where
    safeConvert = return . T.pack . show

instance Convertible T.Text T.Text where
    safeConvert = return

instance Convertible Double T.Text where
    safeConvert = return . T.pack . show

instance Convertible TimeZone T.Text where
    safeConvert = return . T.pack . show

instance Convertible a T.Text => Convertible (Maybe a) T.Text where
    safeConvert (Just x) = safeConvert x
    safeConvert Nothing = return $ T.pack ""

instance Convertible Bool T.Text where
    safeConvert True = return $ T.pack "True"
    safeConvert False = return $ T.pack "False"

instance Convertible Day T.Text where
    safeConvert = return . T.pack . show


instance Typeable a => Convertible C.ByteString (Key a) where
    safeConvert x =
        case readMay (C.unpack x) of
            Just r -> return $ Key r
            Nothing -> convError "Couldn't Convert to Bytestring to Key" x


instance Convertible C.ByteString Bool where
    safeConvert x = case readMay (C.unpack x) of
        Just r -> return r
        Nothing -> convError "Bytestring to Bool Error" x

instance Convertible C.ByteString T.Text where
    safeConvert x = return $ decodeUtf8 x

instance Convertible C.ByteString UTCTime where
    safeConvert x = case readMay (C.unpack x) of
        Just r -> return r
        Nothing -> convError "ByteString to UTCTime Error" x


instance Convertible C.ByteString Day where
    safeConvert x = case readMay (C.unpack x) of
        Just r -> return r
        Nothing -> convError "ByteString to Day Error" x

instance Convertible Int T.Text where
    safeConvert = return . T.pack . show

instance Convertible C.ByteString Int where
    safeConvert x = read $ C.unpack x

instance Convertible Day Double where
    safeConvert x = return $ fromRational $ toRational $ toModifiedJulianDay x

instance Convertible LocalTime Double where
    safeConvert = safeConvert . localTimeToUTC utc


instance Convertible Day Integer where
    safeConvert x = return $ toModifiedJulianDay x

instance Convertible Day Int where
    safeConvert x = return $ fromIntegral $ toModifiedJulianDay x



instance Convertible (Maybe UTCTime) Double where
    safeConvert Nothing = return 999999999999999999999
    safeConvert (Just x) =
        return $ fromRational . toRational . utcTimeToPOSIXSeconds $ x
