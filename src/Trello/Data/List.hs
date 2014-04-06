module Trello.Data.List where
import Data.ByteString.Lazy (ByteString)

import Trello.Data
import Trello.ApiData

getList :: ListRef -> Either Error List
getList listRef = apiGetListById listRef >>= parseList

parseList :: ByteString -> Either Error List
parseList json = Left $ Error "Stub"

parseLists :: ByteString -> Either Error [List]
parseLists json = Left $ Error "Stub"

apiGetListById :: ListRef -> Either Error ByteString
apiGetListById (ListRef listId) = Left $ Error "Stub"
