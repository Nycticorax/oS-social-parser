module Time where

import qualified Data.Text                as T
import           Data.Time
import           Data.Time.Clock.POSIX    (POSIXTime, posixSecondsToUTCTime)
import           Data.Time.Format.ISO8601


isoOrCustom :: T.Text -> Either T.Text UTCTime
isoOrCustom s =
    let utc = iso8601ParseM . T.unpack $ s :: Maybe UTCTime
        zoned = iso8601ParseM . T.unpack $ s :: Maybe ZonedTime
    in  case utc of
        Just parsed -> Right parsed
        Nothing -> case zoned of
            Nothing -> toUTC s possibleFormats
            Just r  -> Right . zonedTimeToUTC $ r
    where
        possibleFormats = ["%a, %d %b %Y T %z", "%Y-%b-%dT%z","%a, %d %b %Y T %EZ", "%Y-%m-%d", "%FT%T", "%Y-%b-%dT%H:%M:%S", "%Y-%b-%dT%H:%M"] :: [String]
        toUTC s [] = Left s
        toUTC s (f:fs) = case parseTimeM True defaultTimeLocale f (T.unpack s) of
            Nothing     -> toUTC s fs
            Just parsed -> Right parsed

unixToUTC x = let x' = x :: POSIXTime in posixSecondsToUTCTime x'
