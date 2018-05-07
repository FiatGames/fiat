{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving         #-}

module FiatGame.TicTacToe where

import           ClassyPrelude.Yesod hiding (unpack)
import           Control.Lens
import           FiatGame.Class
import qualified FiatGame.HttpClient as H

newtype Settings = Settings Value
makeWrapped ''Settings
deriving instance ToJSON Settings
deriving instance FromJSON Settings

instance FiatGame Settings where
  type Move Settings = Value
  type State Settings = Value
  type ClientState Settings = Value
  type ClientSettings Settings = Value
  type Environment Settings = H.ClientBaseUrl

  defaultSettings = ask >>= fmap Settings . liftIO . H.defaultSettings
  addPlayer p (Settings s) = do
    url <- ask
    s' <- liftIO $ H.addPlayer url p s
    pure $ fmap Settings s'
  initialGameState (Settings s) = do
    url <- ask
    ms <- liftIO $ H.initialGameState url s
    pure $ ms & _Right._1 %~ Settings
  makeMove p (Settings s) gs mv = do
    url <- ask
    liftIO $ H.makeMove url p s gs mv
  isPlayersTurn p (Settings s) gs mv = do
    url <- ask
    liftIO $ H.isPlayersTurn url p s gs mv
  isMoveValid p (Settings s) gs mv = do
    url <- ask
    liftIO $ H.isMoveValid url p s gs mv
  toClientSettingsAndState p (Settings s) mgs = do
    url <- ask
    liftIO $ H.toClientSettingsAndState url p s mgs
