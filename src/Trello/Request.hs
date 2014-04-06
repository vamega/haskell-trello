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

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad hiding (join)
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List
import Network.HTTP.Base
import Network.HTTP.Conduit

base_url = "https://api.trello.com/1/"
board_path  = "boards"
list_path   = "lists"
card_path   = "cards"
member_path = "members"
filter_path = "filter"

default_params = [("lists", "all"),
                  ("members", "all")]

type HttpParams = [(String, String)]


getBoardById  oauth (BoardRef id) = api oauth [board_path, id]
getCardById   oauth (CardRef id) = api oauth [card_path, id]
getListById   oauth (ListRef id) = api oauth [list_path, id]
getMemberById oauth (MemberRef id) = api oauth [member_path, id]

getCardsByBoardId   oauth (BoardRef id) Nothing = api oauth [board_path, id, card_path]
getCardsByBoardId   oauth (BoardRef id) (Just filter) = api oauth [board_path, id, card_path, filter_path, show(filter)]
getListsByBoardId   oauth (BoardRef id) Nothing = api oauth [board_path, id, list_path]
getListsByBoardId   oauth (BoardRef id) (Just filter) = api oauth [board_path, id, list_path, filter_path, show(filter)]
getMembersByBoardId oauth (BoardRef id) Nothing = api oauth [board_path, id, member_path]
getMembersByBoardId oauth (BoardRef id) (Just filter) = api oauth [board_path, id, member_path, filter_path, show(filter)]

api :: MonadIO m => OAuth -> [String] -> m ByteString
api oauth path = api' oauth path default_params

api' :: MonadIO m => OAuth -> [String] -> [(String, String)] -> m ByteString
api' oauth path params = lift $ simpleHttp requestURL
  where requestURL = base_url ++ (intercalate "/" path) ++ "?" ++ (urlEncodeVars params) ++ (urlEncodeVars $ oauthList oauth)

oauthList :: OAuth -> [(String, String)]
oauthList (OAuth key token) = [("key", key),("token", token)]
