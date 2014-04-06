module Trello.Api (
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad

import Trello.ApiData
import Trello.Data
import Trello.Request

import Trello.Data.Board
import Trello.Data.Card
import Trello.Data.List
import Trello.Data.Member

board :: MonadIO m => OAuth -> BoardRef -> m (Either Error Board)
board oauth ref = liftM parseBoard $ getBoardById oauth ref

card :: MonadIO m => OAuth -> CardRef -> m (Either Error Card)
card oauth ref = liftM parseCard $ getCardById oauth ref

list :: MonadIO m => OAuth -> ListRef -> m (Either Error List)
list oauth ref = liftM parseList $ getListById oauth ref

member :: MonadIO m => OAuth -> MemberRef -> m (Either Error Member)
member oauth ref = liftM parseMember $ getMemberById oauth ref

listsForBoard :: MonadIO m => OAuth -> Maybe ListFilter -> Board -> m (Either Error [List])
listsForBoard oauth filter (Board ref _ _ _) = liftM parseLists $ getListsByBoardId oauth ref filter

cardsForBoard :: MonadIO m => OAuth -> Maybe CardFilter -> Board -> m (Either Error [Card])
cardsForBoard oauth filter (Board ref _ _ _) = liftM parseCards $ getCardsByBoardId oauth ref filter

membersForBoard :: MonadIO m => OAuth -> Maybe MemberFilter -> Board -> m (Either Error [Member])
membersForBoard oauth filter (Board ref _ _ _) = liftM parseMembers $ getMembersByBoardId oauth ref filter

listFromCard :: MonadIO m => OAuth -> Card -> m (Either Error List)
listFromCard oauth (Card _ _ ref _ _ _ _ _ _) = liftM parseList $ getListById oauth ref

boardFromList :: MonadIO m => OAuth -> List -> m (Either Error List)
boardFromList oauth (List _ ref _ _) = liftM parseList $ getBoardById oauth ref
