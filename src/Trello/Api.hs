module Trello.Api (
) where

import Control.Monad.IO.Class (MonadIO)

import Trello.ApiData
import Trello.Data
import Trello.Request

import Trello.Data.Board
import Trello.Data.Card
import Trello.Data.List
import Trello.Data.Member

board :: OAuth -> BoardRef -> Either Error Board
board oauth ref = getBoardById oauth ref >>= parseBoard

card :: OAuth -> CardRef -> Either Error Card
card oauth ref = getCardById oauth ref >>= parseCard

list :: OAuth -> ListRef -> Either Error List
list oauth ref = getListById oauth ref >>= parseList

member :: OAuth -> MemberRef -> Either Error Member
member oauth ref = getMemberById oauth ref >>= parseMember

listsForBoard :: OAuth -> Board -> Maybe ListFilter -> Either Error [List]
listsForBoard oauth (Board ref _ _ _) filter = getListsByBoardId oauth ref filter >>= parseLists

cardsForBoard :: OAuth -> Board -> Maybe CardFilter -> Either Error [Card]
cardsForBoard oauth (Board ref _ _ _) filter = getCardsByBoardId oauth ref filter >>= parseCards

membersForBoard :: OAuth -> Board -> Maybe MemberFilter -> Either Error [Member]
membersForBoard oauth (Board ref _ _ _) filter = getMembersByBoardId oauth ref filter >>= parseMembers

listFromCard :: OAuth -> Card -> Either Error List
listFromCard oauth (Card _ _ ref _ _ _ _ _ _) = getListById oauth ref >>= parseList

boardFromList :: MonadIO m => OAuth -> List -> m (Either Error List)
boardFromList oauth (List _ ref _ _) = getBoardById oauth ref >>= parseList
