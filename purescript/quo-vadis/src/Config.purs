module Config where

import Prelude

import Data.Either (Either, note)
import Data.Int (fromString)
import Data.Map (Map, lookup)

foreign import _api :: String
foreign import _websocket :: String

type Config = 
  { api :: String
  , websocket :: String
  , userId :: Int
  , joinCode :: String
  , gameId :: Int
  }

getConfig :: Map String String -> Either String Config
getConfig params = do
  userId <- note "userId not in querystring" $ lookup "userId" params >>= fromString
  gameId <- note "gameId not in querystring" $ lookup "gameId" params >>= fromString
  joinCode <- note "joinCode not in querystring" $ lookup "joinCode" params
  pure {api: _api, websocket: _websocket, joinCode, userId, gameId }