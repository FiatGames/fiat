{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module DTO.GameDetail where

import           ClassyPrelude.Yesod
import           Data.Aeson.TH
import           Data.Aeson.Types    (defaultOptions)
import           DTO.Player          (Player (..))

data GameDetail = GameDetail
  { players :: [Player]
  }
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''GameDetail)

