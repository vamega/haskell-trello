-- https://api.trello.com/1/boards/4eea4ffc91e31d1746000046?lists=open&list_fields=name&fields=name,desc&key=[application_key]&token=[optional_auth_token]
module Request where
import Network.HTTP.Conduit
import Trello.Data
import Network.HTTP.Base
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy.Char8 as B

instance FromJSON Board where
     parseJSON (Object v) = Board <$>
                            v .: "name" <*>
                            v .: "age"
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = mzero

board_url = "https://api.trello.com/1/boards/"

test_id = "4d5ea62fd76aa1136000000c"
test_params = [("lists", "all"),
               ("members", "all"),
               ("list_fields", "name"),
               ("fields", "name,desc"),
               ("key", "0cc879616fe466ceb58bd6ee6fc73b8b")]
               --("token", "[optional_auth_token]")]

type HttpParams = [(String, String)]

getBoard :: MonadIO m => String -> HttpParams -> m B.ByteString
getBoard boardId params = simpleHttp requestURL
  where requestURL = board_url ++ boardId ++ "?" ++ (urlEncodeVars params)

main = do
  board <- getBoard test_id test_params
  B.putStrLn board
