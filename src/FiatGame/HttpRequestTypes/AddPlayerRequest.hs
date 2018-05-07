{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module FiatGame.HttpRequestTypes.AddPlayerRequest where

import           ClassyPrelude.Yesod
import           Data.Aeson
import           Data.Aeson.TH
import           FiatGame.Types

data AddPlayerRequest = AddPlayerRequest
  { player   :: FiatPlayer
  , settings :: Value
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''AddPlayerRequest)
