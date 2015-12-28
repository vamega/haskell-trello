{-# LANGUAGE OverloadedStrings #-}
module Trello.Data.Board where
import Control.Applicative ((<$>), (<*>))
import Control.Monad

import Trello.ApiData
import Trello.Data
import Trello.Data.List
import Trello.Data.Member

import Data.Aeson           (decode)
import Data.Aeson.Parser
import Data.Aeson.Types     hiding (Error)
import Data.ByteString.Lazy (ByteString)

parseBoard :: ByteString -> Either Error Board
parseBoard json = validateJson $ decode json

parseBoards :: ByteString -> Either Error [Board]
parseBoards json = validateJson $ decode json

instance FromJSON Board where
  parseJSON (Object v) =
    Board <$> liftM BoardRef (v .: "id")
          <*> v .: "name"
          <*> v .:? "lists"
          <*> v .:? "members"
  parseJSON _          = mzero
