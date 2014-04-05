-- https://api.trello.com/1/boards/4eea4ffc91e31d1746000046?lists=open&list_fields=name&fields=name,desc&key=[application_key]&token=[optional_auth_token]
module Request where
import Network.HTTP.Conduit
import Trello.Data
import Network.HTTP.Base

BOARD_URL = "https://api.trello.com/1/boards/"

test_id = 4eea4ffc91e31d1746000046
test_params = [("lists", "open"),
          ("list_fields", "name"),
          ("fields", "name,desc"),
          ("key", "0cc879616fe466ceb58bd6ee6fc73b8b"),
          ("token", "[optional_auth_token]")]

type HttpParams = [(String, String)]

getBoard :: Int -> HttpParams -> Maybe Board
getBoardURL boardId params = BOARD_URL ++ boardId ++ "?" ++ urlEncodeVars


main = do
  putStrLn $ getBoardURL test_id test_params
  --simpleHttp "http://www.example.com/foo.txt" >>= L.writeFile "foo.txt"

--simpleHTTP (getRequest "http://www.haskell.org/") >>= fmap (take 100) . getResponseBody
