{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Queries.Chat where

import           Database.Esqueleto
import           DTO.ChatCreate        (ChatCreate (..))
import qualified DTO.ChatCreate        as ChatCreate
import           DTO.ChatMessageDetail (ChatMessageDetail, mkChatMessageDetail)
import           DTO.ChatRoomDetail    (ChatRoomDetail, mkChatRoomDetail)
import           Import                hiding (delete, on, update, (=.), (==.))

getGeneralChat :: Key Game -> DB (Maybe (Entity ChatRoom))
getGeneralChat gId = fmap headMay $ select $
  from $ \cr -> do
  where_ (cr^.ChatRoomGameId ==. val gId &&. cr^.ChatRoomIsGeneral ==. val True)
  pure cr

addUserToGeneralChat :: Key User -> Key Game -> DB ()
addUserToGeneralChat userId gId = getGeneralChat gId >>= \case
    Nothing -> pure ()
    Just (Entity cId _) -> void $ insert $ ChatParticipants userId cId

getChatRoomsForUserForGame :: Key User -> Key Game -> DB [Entity ChatRoom]
getChatRoomsForUserForGame userId gId = select $
  from $ \(cr,cp) -> do
  where_ (cr^.ChatRoomGameId ==. val gId &&. cp^.ChatParticipantsUserId ==. val userId)
  pure cr

getAllMessagesForUserForGame :: Key User -> Key Game -> DB [ChatMessageDetail]
getAllMessagesForUserForGame userId gId = do
  msgs <- select $
    from $ \(cr,cm) -> do
    where_ ( cr^.ChatRoomGameId ==. val gId &&. cr^.ChatRoomId ==. cm^.ChatMessageChatRoomId )
    pure cm
  makeMyMessageDetails userId $ map entityKey msgs

isUserInChatRoom :: Key ChatRoom -> Key User -> DB Bool
isUserInChatRoom chatId userId = fmap (not . null) $ select $
  from $ \(cr,cp) -> do
  where_ (cr^.ChatRoomId ==. val chatId &&. cp^.ChatParticipantsUserId ==. val userId)
  pure ()

createChat :: Key User -> ChatCreate -> DB (ChatRoomDetail, Key ChatMessage)
createChat userId cr = do
  t <- liftIO getCurrentTime
  cr' <- insertEntity (ChatRoom (ChatCreate.name cr) (ChatCreate.gameId cr) False)
  mapM_ (\i -> void $ insertUnique (ChatParticipants i (entityKey cr'))) (userId:ChatCreate.users cr)
  cm <- insertEntity $ ChatMessage (ChatCreate.initialMsg cr) t userId (entityKey cr')
  pure (mkChatRoomDetail cr', entityKey cm)

markMessageRead :: Key ChatMessage -> Key User -> DB ()
markMessageRead msgId userId = void $ insertUnique (ChatMessageRead userId msgId)

makeMyMessageDetails :: Key User -> [Key ChatMessage] -> DB [ChatMessageDetail]
makeMyMessageDetails userId msgIds = do
  msgs <- select $ distinct $
    from $ \(cm `InnerJoin` cr `InnerJoin` cp `InnerJoin` u `LeftOuterJoin` cmr) -> do
    on (   just (cm^.ChatMessageId) ==. cmr?.ChatMessageReadChatMessageId
       &&. just (val userId) ==. cmr?.ChatMessageReadUserId
       )
    on ( cp^.ChatParticipantsUserId ==.  val userId )
    on ( cm^.ChatMessageChatRoomId ==. cp^.ChatParticipantsChatRoomId )
    on ( cm^.ChatMessageChatRoomId ==.  cr^.ChatRoomId )
    where_
      (   cm^.ChatMessageId `in_` valList msgIds )
    pure (cr,cm,u,cmr)
  pure $ map (\(a,b,c,d) -> mkChatMessageDetail a b c d) msgs

