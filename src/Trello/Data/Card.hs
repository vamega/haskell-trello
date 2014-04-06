{-# LANGUAGE OverloadedStrings #-}
module Trello.Data.Card where
import Control.Applicative
import Control.Monad
import Data.Aeson (decode)
import Data.Aeson.Parser
import Data.Aeson.Types hiding (Error)
import Data.ByteString.Lazy (ByteString)

import Trello.Data
import Trello.ApiData

newtype CardList = CardList [Card]

getCard :: CardRef -> Either Error Card
getCard cardRef = apiGetCardById cardRef >>= parseCard

parseCard :: ByteString -> Either Error Card
parseCard json = validateJson $ (decode json :: Maybe Card)

parseCards :: ByteString -> Either Error [Card]
parseCards json = validateJson $ (decode json :: Maybe [Card])

apiGetCardById :: CardRef -> Either Error ByteString
apiGetCardById (CardRef cardId) = Left $ Error "Stub"

instance FromJSON Card where
  parseJSON (Object o) =
    Card <$> liftM CardRef  (o .: "id")
         <*> liftM BoardRef (o .: "idBoard")
         <*> liftM ListRef  (o .: "idList")
         <*> o .: "name"
         <*> o .: "desc"
         <*> liftM (map MemberRef) (o .: "idMembers")
         <*> o .:? "due"
         <*> o .: "dateLastActivity"
         <*> o .: "closed"
  parseJSON _          = fail "Can't decode"
