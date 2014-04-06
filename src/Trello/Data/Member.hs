module Trello.Data.Member where
import Data.ByteString.Lazy (ByteString)

import Trello.Data
import Trello.ApiData

getMember :: MemberRef -> Either Error Member
getMember memberRef = apiGetMemberById memberRef >>= parseMember

parseMember :: ByteString -> Either Error Member
parseMember json = Left $ Error "Stub"

parseMembers :: ByteString -> Either Error [Member]
parseMembers json = Left $ Error "Stub"

apiGetMemberById :: MemberRef -> Either Error ByteString
apiGetMemberById (MemberRef memberId) = Left $ Error "Stub"
