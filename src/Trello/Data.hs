module Trello.Data where

import Data.Data
import Data.Time

newtype BoardRef      = BoardRef String deriving (Show, Eq, Ord)
newtype ListRef       = ListRef String deriving (Show, Eq, Ord)
newtype CardRef       = CardRef String deriving (Show, Eq, Ord)
newtype MemberRef     = MemberRef String deriving (Show, Eq, Ord)
newtype CommentRef    = CommentRef String deriving (Show, Eq, Ord)
newtype ChecklistRef  = ChecklistRef String deriving (Show, Eq, Ord)
newtype AttachmentRef = AttachmentRef String deriving (Show, Eq, Ord)

data OAuth = OAuth {
  oauthKey    :: String
  ,oauthToken :: String
} deriving (Show, Eq, Ord)

data Board = Board {
   boardRef     :: BoardRef
  ,boardName    :: String
  ,boardLists   :: Maybe [List]
  ,boardMembers :: Maybe [Member]
} deriving (Show, Eq, Ord)

data List = List {
   listRef    :: ListRef
  ,listBoard  :: BoardRef
  ,listName   :: String
  ,listClosed :: Bool
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
  ,cardClosed      :: Bool
  ,cardShortUrl    :: String
  ,cardUrl         :: String
} deriving (Show, Eq, Ord)

data Member = Member {
   memberRef           :: MemberRef
  ,memberFullName      :: String
  ,memberUsername      :: String
  ,memberBoards        :: Maybe [BoardRef]
  ,memberBoardsInvited :: Maybe [BoardRef]
  ,memberBoardsPinned  :: Maybe [BoardRef]
  ,memberEmail         :: Maybe String
  ,memberUrl           :: Maybe String
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
