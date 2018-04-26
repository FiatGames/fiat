{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module DTO.Player where

import           ClassyPrelude.Yesod
import           Data.Aeson.TH
import           Data.Aeson.Types    (defaultOptions)
import           Model               (User, userIdent)

data Player = Player
  { id   :: Key User
  , name :: Text
  }
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''Player)

mkPlayer :: Entity User -> Player
mkPlayer (Entity k v) = Player k (userIdent v)
