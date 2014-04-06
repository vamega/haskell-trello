{-# LANGUAGE OverloadedStrings #-}
module Trello.Data.Member where
import Control.Applicative
import Control.Monad
import Data.Aeson (decode)
import Data.Aeson.Parser
import Data.Aeson.Types hiding (Error)
import Data.ByteString.Lazy (ByteString)

import Trello.Data
import Trello.ApiData

getMember :: MemberRef -> Either Error Member
getMember memberRef = apiGetMemberById memberRef >>= parseMember

parseMember :: ByteString -> Either Error Member
parseMember json = validateJson $ (decode json :: Maybe Member)

parseMembers :: ByteString -> Either Error [Member]
parseMembers json = validateJson $ (decode json :: Maybe [Member])

apiGetMemberById :: MemberRef -> Either Error ByteString
apiGetMemberById (MemberRef memberId) = Left $ Error "Stub"

instance FromJSON Member where
  parseJSON (Object o) =
    Member <$> liftM MemberRef  (o .: "id")
           <*> liftM (map BoardRef) (o .: "idBoards")
           <*> liftM (map BoardRef) (o .: "idBoardsInvited")
           <*> liftM (map BoardRef) (o .: "idBoardsPinned")
           <*> o .: "fullName"
           <*> o .: "username"
           <*> o .: "email"
           <*> o .: "url"
  parseJSON _          = fail "Can't decode"
