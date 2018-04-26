{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module DTO.ChatMessageDetail where

import           ClassyPrelude.Yesod hiding (id)
import           Data.Aeson.TH
import           Data.Aeson.Types    (defaultOptions)
import           DTO.ChatRoomDetail  (ChatRoomDetail, mkChatRoomDetail)
import           DTO.Player          (Player, mkPlayer)
import           Model               (ChatMessage, ChatMessageRead, ChatRoom,
                                      User, chatMessageMessage, chatMessageSent)

data ChatMessageDetail = ChatMessageDetail
  { read      :: Bool
  , sentOn    :: UTCTime
  , sentBy    :: Player
  , message   :: Text
  , messageId :: Key ChatMessage
  , room      :: ChatRoomDetail
  }
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''ChatMessageDetail)

mkChatMessageDetail :: Entity ChatRoom -> Entity ChatMessage -> Entity User -> Maybe (Entity ChatMessageRead) -> ChatMessageDetail
mkChatMessageDetail cr (Entity k v) u mcmr = ChatMessageDetail
  { read= isJust mcmr
  , sentOn= chatMessageSent v
  , sentBy= mkPlayer u
  , message= chatMessageMessage v
  , messageId= k
  , room= mkChatRoomDetail cr
  }
