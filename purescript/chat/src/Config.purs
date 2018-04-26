module Config where

import Prelude

import Data.Either (Either, note)
import Data.Int (fromString)
import Data.Map (Map, lookup)
import Network.HTTP.Affjax (URL)

foreign import _api :: String
foreign import _websocket :: String

type Config = 
  { api :: URL
  , websocket :: String
  , gameId :: Int
  , userId :: Int
  }

getConfig :: Map String String -> Either String Config
getConfig params = do
  gameId <- note "gameId not in querystring" $ lookup "gameId" params >>= fromString
  userId <- note "userId not in querystring" $ lookup "userId" params >>= fromString
  pure { api: _api, websocket: _websocket, gameId, userId }