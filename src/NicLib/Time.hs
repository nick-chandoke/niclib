-- | simple convenience methods made because reorienting oneself to time's functions for converting among types (e.g. UTCTime and Day)
module NicLib.Time
( today
) where

import RIO
import Data.Time.Calendar
import Data.Time.Clock

-- | (year, month, day)
today :: MonadIO m => m (Integer, Int, Int)
today = liftIO $ toGregorian . utctDay <$> getCurrentTime
