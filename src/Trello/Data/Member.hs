module Trello.Data.Member where
import Trello.Data
import Trello.ApiData

getMember :: MemberRef -> Either Error Member
getMember memberRef = apiGetMemberById memberRef >>= parseMember

parseMember :: String -> Either Error Member
parseMember json = Left $ Error "Stub"

parseMembers :: String -> Either Error [Member]
parseMembers json = Left $ Error "Stub"

apiGetMemberById :: MemberRef -> Either Error String
apiGetMemberById (MemberRef memberId) = Left $ Error "Stub"
