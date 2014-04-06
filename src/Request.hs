{-# LANGUAGE OverloadedStrings #-}

module Request where
import Network.HTTP.Conduit
import Trello.Data
import Trello.Data.Board
import Network.HTTP.Base
import Control.Monad.IO.Class (MonadIO)
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

--decode' :: MonadIO m => m BS.ByteString -> m (Maybe Board)
--decode' = liftM decode 

main :: IO ()
main = do
  board <- getBoard' test_id test_params
  print (decode $ board :: Maybe Board)
  --case board of Nothing -> return Nothing
  --              Just b  -> return . Just $ putStrLn (show b)

