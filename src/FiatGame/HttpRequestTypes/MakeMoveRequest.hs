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

module FiatGame.HttpRequestTypes.MakeMoveRequest where

import           ClassyPrelude.Yesod
import           Data.Aeson
import           Data.Aeson.TH
import           FiatGame.Types

data MakeMoveRequest = MakeMoveRequest
  { player   :: FiatPlayer
  , settings :: Value
  , state    :: Maybe (GameState Value Value)
  , move     :: Maybe Value
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''MakeMoveRequest)
