{-# LANGUAGE OverloadedStrings #-}
module Trello.Data.List where
import Control.Applicative
import Control.Monad
import Data.Aeson (decode)
import Data.Aeson.Parser
import Data.Aeson.Types hiding (Error)
import Data.ByteString.Lazy (ByteString)

import Trello.Data
import Trello.ApiData

getList :: ListRef -> Either Error List
getList listRef = apiGetListById listRef >>= parseList

parseList :: ByteString -> Either Error List
parseList json = validateJson $ (decode json :: Maybe List)

parseLists :: ByteString -> Either Error [List]
parseLists json = validateJson $ (decode json :: Maybe [List])

apiGetListById :: ListRef -> Either Error ByteString
apiGetListById (ListRef listId) = Left $ Error "Stub"

instance FromJSON List where
  parseJSON (Object o) =
    List <$> liftM ListRef  (o .: "id")
         <*> liftM BoardRef (o .: "idBoard")
         <*> o .: "name"
         <*> o .: "closed"
  parseJSON _          = fail "Can't decode"
