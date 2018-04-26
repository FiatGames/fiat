{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module DTO.ChatCreate where

import           ClassyPrelude.Yesod
import           Data.Aeson.TH
import           Data.Aeson.Types    (defaultOptions)
import           Model               (Game, User)

data ChatCreate = ChatCreate
  { gameId     :: Key Game
  , users      :: [Key User]
  , name       :: Text
  , initialMsg :: Text
  }
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''ChatCreate)
