{-# LANGUAGE OverloadedStrings #-}

module Request where
import Network.HTTP.Conduit
import Data.Conduit 
import Trello.Data
import Trello.Data.Board
import Network.HTTP.Base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BS

board_url = "https://api.trello.com/1/boards/"

test_id = "4d5ea62fd76aa1136000000c"
test_params = [("lists", "all"),
               ("members", "all"),
               ("key", "KEY HERE")]
               --("token", "[optional_auth_token]")]

type HttpParams = [(String, String)]

getBoard' :: MonadIO m => String -> HttpParams -> m BS.ByteString
getBoard' boardId params = simpleHttp requestURL
  where requestURL = board_url ++ boardId ++ "?" ++ (urlEncodeVars params)

getBoard'' boardId params =
  do
    request <- parseUrl requestURL
    withManager $ \manager ->
      do
        Response _ _ bsrc <- http request manager
        return bsrc
  where requestURL = board_url ++ boardId ++ "?" ++ (urlEncodeVars params)

main :: IO ()
main = do
  board <- getBoard'' test_id test_params
  print (decode $ board :: Maybe Board)

