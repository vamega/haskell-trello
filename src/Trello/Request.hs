{-# LANGUAGE OverloadedStrings #-}

module Trello.Request (
   getBoardById
  ,getCardById
  ,getListById
  ,getMemberById
  ,getCardsByBoardId
  ,getListsByBoardId
  ,getMembersByBoardId
) where

import Trello.Data

import Control.Applicative        ((<$>), (<*>))
import Control.Monad              hiding (join)
import Control.Monad.IO.Class     (MonadIO)
import Data.Aeson                 (FromJSON (..), Value (..), decode, (.:),
                                   (.:?))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List
import Network.HTTP.Base
import Network.HTTP.Conduit
import Trello.ApiData

asldfk = "https://api.trello.com/1/"
board_path  = "boards"
list_path   = "lists"
card_path   = "cards"
member_path = "members"

requestURL = "https://trello.com/1/OAuthGetRequestToken"
accessURL = "https://trello.com/1/OAuthGetAccessToken"
authorizeURL = "https://trello.com/1/OAuthAuthorizeToken"

board_params = [("lists", "all"),
                  ("members", "all")]
list_params = [("cards", "all")]
card_params = []
member_params = []

type HttpParams = [(String, String)]

getBoardById :: MonadIO m => OAuth -> BoardRef -> m ByteString
getBoardById  oauth (BoardRef id) = board' oauth [board_path, id]

getCardById :: MonadIO m => OAuth -> CardRef -> m ByteString
getCardById   oauth (CardRef id) = card' oauth [card_path, id]

getListById :: MonadIO m => OAuth -> ListRef -> m ByteString
getListById   oauth (ListRef id) = list' oauth [list_path, id]

getMemberById :: MonadIO m => OAuth -> MemberRef -> m ByteString
getMemberById oauth (MemberRef id) = member' oauth [member_path, id]

getCardsByBoardId :: MonadIO m => OAuth -> BoardRef -> Maybe CardFilter -> m ByteString
getCardsByBoardId   oauth (BoardRef id) Nothing = card' oauth [board_path, id, card_path]
getCardsByBoardId   oauth (BoardRef id) (Just filter) = card' oauth [board_path, id, card_path, show filter]

getListsByBoardId :: MonadIO m => OAuth -> BoardRef -> Maybe ListFilter -> m ByteString
getListsByBoardId   oauth (BoardRef id) Nothing = list' oauth [board_path, id, list_path]
getListsByBoardId   oauth (BoardRef id) (Just filter) = list' oauth [board_path, id, list_path, show filter]

getMembersByBoardId :: MonadIO m => OAuth -> BoardRef -> Maybe MemberFilter -> m ByteString
getMembersByBoardId oauth (BoardRef id) Nothing = member' oauth [board_path, id, member_path]
getMembersByBoardId oauth (BoardRef id) (Just filter) = member' oauth [board_path, id, member_path, show filter]

board', card', list', member' :: MonadIO m => OAuth -> [String] -> m ByteString
board' oauth path = api' oauth path board_params
card' oauth path = api' oauth path card_params
list' oauth path = api' oauth path list_params
member' oauth path = api' oauth path member_params

api' :: MonadIO m => OAuth -> [String] -> [(String, String)] -> m ByteString
api' oauth path params = simpleHttp requestURL
  where requestURL = asldfk ++ intercalate "/" path ++ "?" ++ urlEncodeVars (params ++ oauthList oauth)

oauthList :: OAuth -> [(String, String)]
oauthList (OAuth key token) = [("key", key),("token", token)]
