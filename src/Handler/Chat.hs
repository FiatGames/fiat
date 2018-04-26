{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Chat where

import qualified Chat.Types            as Chat
import           Data.Aeson            (decode, encode)
import qualified Data.Conduit.List
import           Data.Conduit.TMChan
import qualified Data.Text.Lazy        as TL
import           DTO.ChatCreate        (ChatCreate)
import qualified DTO.ChatCreate        as ChatCreate
import           DTO.ChatMessageDetail (ChatMessageDetail)
import           FiatChannel
import           Handler.Game          (ensureInGame)
import           Import
import           Queries.Chat
import           Queries.Game
import           Yesod.WebSockets      (sinkWSText, sourceWS, webSockets)

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ []       = pure True
allM p (x:xs)   = p x >>= \case
  True -> allM p xs
  False -> pure False

postChatR :: Handler Value
postChatR = do
  app <- getYesod
  (userId,_) <- requireAuthPair
  c <- requireJsonBody :: Handler ChatCreate
  runDB (isUserInGame (ChatCreate.gameId c) userId) >>= \case
    False -> sendStatusJSON forbidden403 ("You aren't in that game" :: Text)
    True -> runDB (allM (isUserInGame (ChatCreate.gameId c)) (ChatCreate.users c)) >>= \case
        False -> sendStatusJSON badRequest400  ("All of those users aren't in the game" :: Text)
        True -> do
          (cr',msgId) <- runDB $ createChat userId c
          liftIO $ withChannel (appChatChannels app) (ChatCreate.gameId c)
                 $ \myChan -> runConduit $ Data.Conduit.List.sourceList [[msgId]] .| sinkTMChan myChan
          returnJson cr'

processFromWebSocket :: Key User -> Text -> Handler (Maybe [Key ChatMessage])
processFromWebSocket userId msg = case decode $ encodeUtf8 $ fromStrict msg of
    Nothing -> pure Nothing
    (Just (Chat.ReadMessage msgId)) -> runDB (markMessageRead msgId userId) >> pure (Just [msgId])
    (Just (Chat.PostMessage chatRoomId chatMsg)) -> runDB (isUserInChatRoom chatRoomId userId) >>= \case
        False -> pure Nothing
        True -> do
            t <- liftIO getCurrentTime
            let cMsg = ChatMessage chatMsg t userId chatRoomId
            msgId <- runDB $ insert cMsg
            pure $ Just [msgId]

processFromChan :: Key User -> [Key ChatMessage] -> Handler (Maybe TL.Text)
processFromChan userId msg = do
    msgDetails <- runDB (makeMyMessageDetails userId msg)
    if null msgDetails
    then pure Nothing
    else pure $ Just $ mkClientMsg msgDetails

mkClientMsg :: [ChatMessageDetail] -> TL.Text
mkClientMsg = decodeUtf8 . encode . Chat.Messages

getGameChatR :: GameId ->  Handler Html
getGameChatR gameId = do
    app <- getYesod
    (userId,_) <- requireAuthPair
    _ <- ensureInGame gameId userId
    oldMsgs <- runDB $ getAllMessagesForUserForGame userId gameId
    webSockets $ withChannel (appChatChannels app) gameId $ \myChan -> do
        runConduit $ Data.Conduit.List.sourceList [mkClientMsg oldMsgs] .| sinkWSText
        race_
            (runConduit $ sourceWS
                       .| Data.Conduit.List.mapM (lift . processFromWebSocket userId)
                       .| Data.Conduit.List.catMaybes
                       .| sinkTMChan myChan
            )
            (runConduit $ sourceTMChan myChan
                       .| Data.Conduit.List.mapM (lift . processFromChan userId)
                       .| Data.Conduit.List.catMaybes
                       .| sinkWSText
            )
    badMethod
