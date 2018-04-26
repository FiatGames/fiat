{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module DTO.ChatRoomDetail where

import           ClassyPrelude.Yesod hiding (id)
import           Data.Aeson.TH
import           Data.Aeson.Types    (defaultOptions)
import           Model               (ChatRoom, chatRoomName)

data ChatRoomDetail = ChatRoomDetail
  { id   :: Key ChatRoom
  , name :: Text
  }
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''ChatRoomDetail)

mkChatRoomDetail :: Entity ChatRoom -> ChatRoomDetail
mkChatRoomDetail (Entity k v) = ChatRoomDetail
  { id= k
  , name= chatRoomName v
  }
