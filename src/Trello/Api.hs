module Trello.Api where
import Data.Time
import System.Locale
import Trello.ApiData

parseTimestamp :: String -> Maybe UTCTime
parseTimestamp time = parseTime defaultTimeLocale "%FT%T%Q%Z" time
