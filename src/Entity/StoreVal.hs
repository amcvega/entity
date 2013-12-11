{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances
  , FlexibleContexts, OverloadedStrings, DeriveDataTypeable #-}
module Entity.StoreVal where
import Data.Time (UTCTime, Day, LocalTime, TimeZone(..), toModifiedJulianDay)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Convertible
import Entity.Definition
import Safe
import Data.Data

import Entity.Convertible ()

import qualified Data.Text as T

data StoreVal = StoreByteString B.ByteString
              | StoreInteger Integer
              | StoreString String
              | StoreText T.Text
              | StoreDouble Double
              | StoreInt Int
              | StoreBool Bool
              | StoreUTCTime UTCTime
              | StoreLocalTime LocalTime
              | StoreTimeZone TimeZone
              | StoreDay Day
              | StoreList [StoreVal]
              | StoreNil
              deriving (Data, Typeable, Eq)

instance Show StoreVal where
    show (StoreByteString x) = show $ B.unpack x
    show (StoreInteger x)    = show x
    show (StoreString x)     = x
    show (StoreDouble x)     = show x
    show (StoreInt x)        = show x
    show (StoreBool x)       = show x
    show (StoreText x)       = show x
    show (StoreUTCTime x)    = show x
    show (StoreDay x)        = show x
    show (StoreLocalTime x)  = show x
    show (StoreTimeZone x)   = show $ timeZoneMinutes x
    show StoreNil            = ""

instance Convertible StoreVal B.ByteString where
    safeConvert (StoreByteString x) = return x
    safeConvert (StoreText x)       = return $ encodeUtf8 x
    safeConvert (StoreInteger x)    = return $ C.pack $ show x
    safeConvert (StoreInt x)        = return $ C.pack $ show x
    safeConvert (StoreString x)     = return $ C.pack x
    safeConvert (StoreDouble x)     = return $ C.pack $ show x
    safeConvert (StoreBool x)       = return $ C.pack $ show x
    safeConvert (StoreUTCTime x)    = return $ C.pack $ show x
    safeConvert (StoreDay x)        = return $ C.pack $ show x
    safeConvert (StoreLocalTime x)  = return $ C.pack $ show x
    safeConvert (StoreTimeZone x)   = return $ C.pack $ show $
                                      timeZoneMinutes x
    safeConvert (StoreList xs)      = return $ C.pack $ show xs
    safeConvert StoreNil            = return ""


instance Convertible B.ByteString StoreVal where
    safeConvert = return . StoreByteString

instance Convertible StoreVal Integer where
    safeConvert (StoreInteger x) = safeConvert x
    safeConvert inp@(StoreByteString x) =  case mint of
        Just (i,_) -> return i
        Nothing -> convError "Couldn't Convert StoreVal to Int" inp
        where
            mint = C.readInteger x

instance Convertible Integer StoreVal where
    safeConvert = return . StoreInteger

instance Convertible StoreVal String where
    safeConvert (StoreString x)     = return x
    safeConvert (StoreByteString x) = return $ C.unpack x
    safeConvert (StoreInteger x)    = return $ show x
    safeConvert (StoreDouble x)     = return $ show x
    safeConvert (StoreInt x)        = return $ show x
    safeConvert (StoreBool x)       = return $ show x
    safeConvert (StoreText x)       = return $ T.unpack x
    safeConvert (StoreDay x)        = return $ show x
    safeConvert (StoreUTCTime x)    = return $ show x
    safeConvert (StoreLocalTime x)  = return $ show x
    safeConvert (StoreTimeZone x)   = return $ show $ timeZoneMinutes x
    safeConvert StoreNil            = return ""

instance Convertible String StoreVal where
    safeConvert = return . StoreString


instance Convertible StoreVal T.Text where
    safeConvert (StoreText x)       = return x
    safeConvert (StoreByteString x) = return $ decodeUtf8 x
    safeConvert (StoreString x)     = return $ T.pack x

instance Convertible T.Text StoreVal where
    safeConvert = return . StoreText

instance Convertible StoreVal Double where
    safeConvert (StoreDouble x) = return  x
    safeConvert (StoreInt x) = return $ fromRational $ toRational x
    safeConvert (StoreUTCTime x) =
        return $ fromRational $ toRational $ utcTimeToPOSIXSeconds x
    safeConvert (StoreDay x) =
        return $ fromInteger $ toModifiedJulianDay x
    safeConvert inp@(StoreByteString x) = case readMay (C.unpack x) of
        Nothing -> convError "Couldn't Convert Storeval to Double" inp
        Just x' -> return x'
    safeConvert x = convError ("It's Type: " ++ show (typeOf x)) x

instance Convertible Double StoreVal where
    safeConvert = return . StoreDouble


instance Convertible StoreVal Int where
    safeConvert (StoreInt x) = return x
    safeConvert inp@(StoreByteString x) = case mint of
        Just (i,_) -> return i
        Nothing -> convError "Couldn't Convert StoreVal to Int" inp
        where
            mint = C.readInt x


instance Convertible Int StoreVal where
    safeConvert = return . StoreInt

instance (Convertible C.ByteString a, Convertible StoreVal a, Convertible T.Text a,
          Typeable a)
         => Convertible StoreVal (Maybe a) where
    -- safeConvert (StoreInt x) =  Just `fmap` safeConvert x
    safeConvert StoreNil = return Nothing
    safeConvert (StoreByteString x) =
        if "" == x
        then return Nothing
        else Just `fmap` safeConvert x
    safeConvert (StoreText x) = Just `fmap` safeConvert x
    safeConvert x = convError "WTF!?!?" x
        -- else case readMay (C.unpack x) :: Maybe Int of
        --     Just r -> Just `fmap` safeConvert r
            -- Nothing -> convError "Couldn't Convert to Maybe Int" inp


instance (Convertible a StoreVal) => Convertible (Maybe a) StoreVal where
    safeConvert x = maybe (return StoreNil) safeConvert x


instance (Typeable a) => Convertible StoreVal (Key a) where
    safeConvert (StoreInt x)    = return $ Key x
    safeConvert (StoreString x) = return $ Key $ read x
    safeConvert (StoreText x)   = return $ Key $ read $ T.unpack x
    safeConvert inp@(StoreByteString x) = case C.readInt x of
        Just (r,_) -> return $ Key r
        Nothing -> convError "Couldn't Convert Storeval to Key" inp



instance Convertible (Key a) StoreVal where
    safeConvert = safeConvert . unKey


instance Convertible Bool StoreVal where
    safeConvert = return . StoreBool

instance Convertible StoreVal Bool where
    safeConvert (StoreBool x) = return x
    safeConvert inp@(StoreString x) =
        case readMay x of
            Just r -> return r
            Nothing -> convError "Conversion StoreVal to Bool Error" inp
    safeConvert inp@(StoreByteString x) =
        case readMay (C.unpack x) of
            Just r -> return r
            Nothing -> convError "Conversion Bytestring to Bool Error" inp


instance Convertible UTCTime StoreVal where
    safeConvert = return . StoreUTCTime

instance Convertible StoreVal UTCTime where
    safeConvert (StoreUTCTime x) = return x
    safeConvert inp@(StoreByteString x) = case readMay (C.unpack x) of
        Just r -> return r
        Nothing -> convError "ByteString to UTCTime Error: " inp

instance Convertible Day StoreVal where
    safeConvert = return . StoreDay

instance Convertible StoreVal Day where
    safeConvert (StoreDay x) = return x
    safeConvert inp@(StoreByteString x) = case readMay (C.unpack x) of
        Just r -> return r
        Nothing -> convError "ByteString to Day Error: " inp


instance Convertible TimeZone StoreVal where
    safeConvert = return . StoreTimeZone


instance Convertible StoreVal TimeZone where
    safeConvert (StoreTimeZone x) = return x
    safeConvert (StoreInt x) = return $ TimeZone x False ""
    safeConvert inp@(StoreByteString x) = case readMay (C.unpack x) of
        Just r -> return $ TimeZone r False ""
        Nothing -> convError "ByteString to TimeZone error: " inp


instance Convertible LocalTime StoreVal where
    safeConvert = return . StoreLocalTime

instance Convertible LocalTime T.Text where
    safeConvert = return . T.pack . show

instance Convertible StoreVal LocalTime where
    safeConvert (StoreLocalTime x) = return x
    safeConvert inp@(StoreByteString x) = case readMay (C.unpack x) of
        Just r -> return r
        Nothing -> convError "ByteString to LocalTime Error: " inp


instance Convertible (Key a) StoreVal => Convertible [Key a] StoreVal where
    safeConvert xs = return $ StoreList $ map toStore xs


instance Typeable a => Convertible StoreVal [Key a] where
    safeConvert (StoreList xs) = return $ map (Key . fromStore) xs
    safeConvert inp@(StoreByteString x) = case readMay (C.unpack x) of
        Just r -> return $ map Key r
        Nothing -> convError "ByteString to [Key a] Error" inp

toStore :: Convertible a StoreVal => a -> StoreVal
toStore = convert

fromStore :: Convertible StoreVal a => StoreVal -> a
fromStore = convert
