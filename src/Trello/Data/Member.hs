{-# LANGUAGE OverloadedStrings #-}
module Trello.Data.Member where
import Control.Applicative
import Control.Monad
import Data.Aeson           (decode)
import Data.Aeson.Parser
import Data.Aeson.Types     hiding (Error)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe

import Trello.ApiData
import Trello.Data

parseMember :: ByteString -> Either Error Member
parseMember json = validateJson $ decode json

parseMembers :: ByteString -> Either Error [Member]
parseMembers json = validateJson $ decode json

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
