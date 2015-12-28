{-# LANGUAGE OverloadedStrings #-}

module Trello.Request (
   getBoardById
 , getCardById
 , getListById
 , getMemberById
 , getCardsByBoardId
 , getListsByBoardId
 , getMembersByBoardId
) where

import           Trello.Data

import           Control.Applicative        ((<$>), (<*>))
import           Control.Monad              hiding (join)
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Aeson                 (FromJSON (..), Value (..), decode, (.:), (.:?))
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.List
import           Network.HTTP.Base
import           Network.HTTP.Conduit
import           Trello.ApiData

baseUrl = "https://api.trello.com/1/"
boardPath  = "boards"
listPath   = "lists"
cardPath   = "cards"
memberPath = "members"

requestURL = "https://trello.com/1/OAuthGetRequestToken"
accessURL = "https://trello.com/1/OAuthGetAccessToken"
authorizeURL = "https://trello.com/1/OAuthAuthorizeToken"

boardParams = [("lists", "all"), ("members", "all")]
listParams = [("cards", "all")]
cardParams = []
memberParams = []

type HttpParams = [(String, String)]

getBoardById :: MonadIO m => OAuth -> BoardRef -> m ByteString
getBoardById  oauth (BoardRef id) = board' oauth [boardPath, id]

getCardById :: MonadIO m => OAuth -> CardRef -> m ByteString
getCardById   oauth (CardRef id) = card' oauth [cardPath, id]

getListById :: MonadIO m => OAuth -> ListRef -> m ByteString
getListById   oauth (ListRef id) = list' oauth [listPath, id]

getMemberById :: MonadIO m => OAuth -> MemberRef -> m ByteString
getMemberById oauth (MemberRef id) = member' oauth [memberPath, id]

getCardsByBoardId :: MonadIO m => OAuth -> BoardRef -> Maybe CardFilter -> m ByteString
getCardsByBoardId   oauth (BoardRef id) Nothing = card' oauth [boardPath, id, cardPath]
getCardsByBoardId   oauth (BoardRef id) (Just filter) = card' oauth [boardPath, id, cardPath, show filter]

getListsByBoardId :: MonadIO m => OAuth -> BoardRef -> Maybe ListFilter -> m ByteString
getListsByBoardId   oauth (BoardRef id) Nothing = list' oauth [boardPath, id, listPath]
getListsByBoardId   oauth (BoardRef id) (Just filter) = list' oauth [boardPath, id, listPath, show filter]

getMembersByBoardId :: MonadIO m => OAuth -> BoardRef -> Maybe MemberFilter -> m ByteString
getMembersByBoardId oauth (BoardRef id) Nothing = member' oauth [boardPath, id, memberPath]
getMembersByBoardId oauth (BoardRef id) (Just filter) = member' oauth [boardPath, id, memberPath, show filter]

board', card', list', member' :: MonadIO m => OAuth -> [String] -> m ByteString
board' oauth path = api' oauth path boardParams
card' oauth path = api' oauth path cardParams
list' oauth path = api' oauth path listParams
member' oauth path = api' oauth path memberParams

api' :: MonadIO m => OAuth -> [String] -> [(String, String)] -> m ByteString
api' oauth path params = simpleHttp requestURL
  where requestURL = baseUrl ++ intercalate "/" path ++ "?" ++ urlEncodeVars (params ++ oauthList oauth)

oauthList :: OAuth -> [(String, String)]
oauthList (OAuth key token) = [("key", key),("token", token)]
