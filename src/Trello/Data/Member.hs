{-# LANGUAGE OverloadedStrings #-}
module Trello.Data.Member where
import Control.Applicative
import Control.Monad
import Data.Aeson (decode)
import Data.Aeson.Parser
import Data.Aeson.Types hiding (Error)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe

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
    Member <$> liftM MemberRef (o .: "id")
           <*> o .: "fullName"
           <*> o .: "username"
           <*> liftM (liftMap BoardRef) (o .:? "idBoards")
           <*> liftM (liftMap BoardRef) (o .:? "idBoardsInvited")
           <*> liftM (liftMap BoardRef) (o .:? "idBoardsPinned")
           <*> o .:? "email"
           <*> o .:? "url"
    where liftMap f = liftM (map f)
  parseJSON _          = fail "Can't decode"
