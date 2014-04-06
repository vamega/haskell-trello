{-# LANGUAGE OverloadedStrings #-}
module Trello.Data.Board where
import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>), (<*>))
import Control.Monad

import Trello.Data
import Trello.ApiData
import Trello.Data.List
import Trello.Data.Card
import Trello.Data.Member

import Data.Aeson (decode)
import Data.Aeson.Parser
import Data.Aeson.Types hiding (Error)

getBoard :: BoardRef -> Either Error Board
getBoard ref = getBoardById ref >>= parseBoard

getListsForBoard :: Board -> ListFilter -> Either Error [List]
getListsForBoard (Board ref _ _ _) filter = getListsByBoardId ref filter >>= parseLists

getCardsForBoard :: Board -> CardFilter -> Either Error [Card]
getCardsForBoard (Board ref _ _ _) filter = getCardsByBoardId ref filter >>= parseCards

getMembersForBoard :: Board -> MemberFilter -> Either Error [Member]
getMembersForBoard (Board ref _ _ _) filter = getMembersByBoardId ref filter >>= parseMembers

parseBoard :: ByteString -> Either Error Board
parseBoard json = Left $ Error "Stub"

getBoardById :: BoardRef -> Either Error ByteString
getBoardById (BoardRef id) = Left $ Error "Stub"

getListsByBoardId :: BoardRef -> ListFilter -> Either Error ByteString
getListsByBoardId (BoardRef id) filter = Left $ Error "Stub"

getCardsByBoardId :: BoardRef -> CardFilter -> Either Error ByteString
getCardsByBoardId (BoardRef id) filter = Left $ Error "Stub"

getMembersByBoardId :: BoardRef -> MemberFilter -> Either Error ByteString
getMembersByBoardId (BoardRef id) filter = Left $ Error "Stub"

instance FromJSON Board where
  parseJSON (Object v) =
    Board <$> liftM BoardRef (v .: "id")
          <*> v .: "name"
          <*> v .:? "lists"
          <*> v .:? "members"
  parseJSON _          = mzero


