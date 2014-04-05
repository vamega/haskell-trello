module Trello.Data.List where
import Trello.Data
import Trello.ApiData

getList :: ListRef -> Either Error List
getList listRef = apiGetListById listRef >>= parseList

parseList :: String -> Either Error List
parseList json = Left $ Error "Stub"

parseLists :: String -> Either Error [List]
parseLists json = Left $ Error "Stub"

apiGetListById :: ListRef -> Either Error String
apiGetListById (ListRef listId) = Left $ Error "Stub"
