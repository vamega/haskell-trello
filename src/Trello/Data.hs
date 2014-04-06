{-# LANGUAGE OverloadedStrings #-}
module Trello.Data where

import Data.Data
import Data.Time
import Data.Aeson.Types
import Control.Applicative
import Control.Monad

newtype BoardRef      = BoardRef String deriving (Show, Eq, Ord)
newtype ListRef       = ListRef String deriving (Show, Eq, Ord)
newtype CardRef       = CardRef String deriving (Show, Eq, Ord)
newtype MemberRef     = MemberRef String deriving (Show, Eq, Ord)
newtype CommentRef    = CommentRef String deriving (Show, Eq, Ord)
newtype ChecklistRef  = ChecklistRef String deriving (Show, Eq, Ord)
newtype AttachmentRef = AttachmentRef String deriving (Show, Eq, Ord)

data Board = Board {
   boardRef     :: BoardRef
  ,boardName    :: String
  ,boardLists   :: [ListRef]
  ,boardMembers :: [MemberRef]
} deriving (Show, Eq, Ord)

data List = List {
   listRef   :: ListRef
  ,listBoard :: BoardRef
  ,listName  :: String
  ,listCards :: [CardRef]
} deriving (Show, Eq, Ord)

data Card = Card {
   cardRef         :: CardRef
  ,cardBoard       :: BoardRef
  ,cardList        :: ListRef
  ,cardName        :: String
  ,cardDescription :: Maybe String
  ,cardMembers     :: [MemberRef]
  ,cardDueDate     :: Maybe UTCTime
  ,cardUpdatedAt   :: UTCTime
  ,cardArchived    :: Bool
} deriving (Show, Eq, Ord)

data Member = Member {
   memberRef    :: MemberRef
  ,memberName   :: String
  ,memberBoards :: [BoardRef]
  ,memberCards  :: [CardRef]
} deriving (Show, Eq, Ord)

data Comment = Comment {
   commentRef    :: CommentRef
  ,commentMember :: MemberRef
  ,commentCard   :: CardRef
  ,commentTime   :: UTCTime
  ,commentText   :: String
} deriving (Show, Eq, Ord)

data Checklist = Checklist {
   checklistRef   :: ChecklistRef
  ,checklistName  :: String
  ,checklistItems :: [ChecklistItem]
  ,checklistCard  :: CardRef
} deriving (Show, Eq, Ord)

data ChecklistItem = ChecklistItem {
   checklistItemDone :: Bool
  ,checklistItemText :: String
} deriving (Show, Eq, Ord)

data Label = Label {
  labelName         :: String
  ,labelDescription :: Maybe String
} deriving (Show, Eq, Ord)

data Attachment = Attachment {
   attachmentRef   ::  AttachmentRef
  ,attachmentName :: String
  ,attachmentUrl  :: String
  ,attachmentTime :: UTCTime
} deriving (Show, Eq, Ord)

instance FromJSON Card where
  parseJSON (Object o) =
    Card <$> liftM CardRef  (o .: "id")
         <*> liftM BoardRef (o .: "idBoard")
         <*> liftM ListRef  (o .: "idList")
         <*> o .: "name"
         <*> o .: "desc"
         <*> liftM (map MemberRef) (o .: "idMembers")
         <*> o .:? "due"
         <*> o .: "dateLastActivity"
         <*> o .: "closed" -- Confirm this mapping later.
  parseJSON _          = fail "Can't decode"

