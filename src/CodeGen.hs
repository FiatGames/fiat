{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module CodeGen where

import qualified Chat.Types                                as Chat
import           Data.Proxy
import qualified DTO.ChatCreate                            as DTO
import qualified DTO.ChatMessageDetail                     as DTO
import qualified DTO.ChatRoomDetail                        as DTO
import qualified DTO.GameDetail                            as DTO
import qualified DTO.Player                                as DTO
import qualified FiatGame.QuoVadis                         as QuoVadis
import qualified FiatGame.ToClient.Types                   as ToClient
import qualified FiatGame.ToServer.Types                   as ToServer
import           FiatGame.Types
import           GameType
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes        (psInt, psString)
import           Language.PureScript.Bridge.TypeParameters (A, B, C)
import           Model
import qualified QuoVadis.Moves                            as QuoVadis
import qualified QuoVadis.Types                            as QuoVadisTypes

bridge :: BridgePart
bridge = defaultBridge <|> int64Bridge <|> integerBridge <|> utcTimeBridge <|> keyBridge <|> charBridge

integerBridge :: BridgePart
integerBridge = typeName ^== "Integer" >> pure psInt

int64Bridge :: BridgePart
int64Bridge = typeName ^== "Int64" >> pure psInt

utcTimeBridge :: BridgePart
utcTimeBridge = typeName ^== "UTCTime" >> pure psString

keyBridge :: BridgePart
keyBridge = typeName ^== "Key" >> pure psInt

charBridge :: BridgePart
charBridge = typeName ^== "Char" >> pure psString

fiatGame :: [SumType 'Haskell]
fiatGame =
  [ mkSumType (Proxy :: Proxy User)
  , mkSumType (Proxy :: Proxy GameType)
  , mkSumType (Proxy :: Proxy GameStage)
  , mkSumType (Proxy :: Proxy Game)
  , mkSumType (Proxy :: Proxy ChatRoom)
  , mkSumType (Proxy :: Proxy ChatMessage)
  , mkSumType (Proxy :: Proxy (FutureMove A))
  , mkSumType (Proxy :: Proxy FiatPlayer)
  , mkSumType (Proxy :: Proxy ToClient.Error)
  , mkSumType (Proxy :: Proxy (ToServer.Cmd A B))
  , mkSumType (Proxy :: Proxy (ToServer.Msg A B))
  , mkSumType (Proxy :: Proxy (ToClient.Msg A B C))
  , mkSumType (Proxy :: Proxy (GameState A B))
  , mkSumType (Proxy :: Proxy FiatGameHash)
  , mkSumType (Proxy :: Proxy Chat.ToServer)
  , mkSumType (Proxy :: Proxy Chat.ToClient)
  , mkSumType (Proxy :: Proxy DTO.GameDetail)
  , mkSumType (Proxy :: Proxy DTO.Player)
  , mkSumType (Proxy :: Proxy DTO.ChatMessageDetail)
  , mkSumType (Proxy :: Proxy DTO.ChatCreate)
  , mkSumType (Proxy :: Proxy DTO.ChatRoomDetail)
  ]

quoVadis :: [SumType 'Haskell]
quoVadis =
  [ mkSumType (Proxy :: Proxy QuoVadisTypes.Laurel)
  , mkSumType (Proxy :: Proxy QuoVadisTypes.PlayerState)
  , mkSumType (Proxy :: Proxy QuoVadisTypes.Edge)
  , mkSumType (Proxy :: Proxy QuoVadis.Move)
  , mkSumType (Proxy :: Proxy QuoVadis.Settings)
  , mkSumType (Proxy :: Proxy QuoVadis.Committee)
  , mkSumType (Proxy :: Proxy QuoVadis.ClientGameState)
  ]

main :: IO ()
main = do
  writePSTypes "purescript/purescript-fiat-game/src" (buildBridge bridge) fiatGame
  writePSTypes "purescript/quo-vadis/src" (buildBridge bridge) quoVadis
