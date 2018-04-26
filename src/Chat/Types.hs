{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Chat.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           DTO.ChatMessageDetail
import           GHC.Generics
import           Import.NoFoundation

data ToClient = Messages
  { msgs :: [ChatMessageDetail]
  }
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''ToClient)

data ToServer
  = PostMessage (Key ChatRoom) Text
  | ReadMessage (Key ChatMessage)
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''ToServer)

