{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
   NamedFieldPuns
 , RecordWildCards
#-}

-- A collection of orphan instances
module Orphans () where

import Control.Monad (liftM)
import Data.Binary (Binary, put, get, Get)
import Data.Time.Calendar (Day (ModifiedJulianDay), toModifiedJulianDay)
import Data.Time.Clock (UTCTime(..))
import Network.HTTP.Client (Cookie(..), CookieJar, createCookieJar, destroyCookieJar, cookie_name, cookie_value, cookie_expiry_time, cookie_domain, cookie_path, cookie_creation_time, cookie_last_access_time, cookie_persistent, cookie_host_only, cookie_secure_only, cookie_http_only)

instance Binary UTCTime where
    put (UTCTime {..}) = do
        put (toModifiedJulianDay utctDay)
        put (truncate utctDayTime :: Integer)
    get = do
        modJulDay <- get :: Get Integer
        dayTimeInt <- get :: Get Integer
        return $ UTCTime (ModifiedJulianDay modJulDay) (fromInteger dayTimeInt)

instance Binary Cookie where
    put (Cookie {..}) = do
        put cookie_name
        put cookie_value
        put cookie_expiry_time
        put cookie_domain
        put cookie_path
        put cookie_creation_time
        put cookie_last_access_time
        put cookie_persistent
        put cookie_host_only
        put cookie_secure_only
        put cookie_http_only
    get = Cookie <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

instance Binary CookieJar where
    put = put . destroyCookieJar
    get = liftM createCookieJar get

