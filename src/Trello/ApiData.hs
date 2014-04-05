module Trello.ApiData where

newtype Error = Error String deriving Show

data ListFilter = ListNone | ListOpen | ListClosed | ListAll
instance Show ListFilter where
  show (ListNone)   = "none"
  show (ListOpen)   = "open"
  show (ListClosed) = "closed"
  show (ListAll)    = "all"

data CardFilter = CardNone | CardVisible | CardOpen | CardClosed | CardAll
instance Show CardFilter where
  show (CardNone)    = "none"
  show (CardVisible) = "visible"
  show (CardOpen)    = "open"
  show (CardClosed)  = "closed"
  show (CardAll)     = "all"

data MemberFilter = MemberNone | MemberNormal | MemberAdmins | MemberOwners | MemberAll
instance Show MemberFilter where
  show (MemberNone)   = "none"
  show (MemberNormal) = "normal"
  show (MemberAdmins) = "admins"
  show (MemberOwners) = "owners"
  show (MemberAll)    = "all"

