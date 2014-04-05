module Trello.Data.Card where
import Trello.Data
import Trello.ApiData

getCard :: CardRef -> Either Error Card
getCard cardRef = apiGetCardById cardRef >>= parseCard

parseCard :: String -> Either Error Card
parseCard json = Left $ Error "Stub"

parseCards :: String -> Either Error [Card]
parseCards json = Left $ Error "Stub"

apiGetCardById :: CardRef -> Either Error String
apiGetCardById (CardRef cardId) = Left $ Error "Stub"
