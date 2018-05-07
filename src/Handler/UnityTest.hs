{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.UnityTest where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)
import           Data.Conduit
import qualified Data.Conduit.List
import qualified Data.Text.Lazy     as TL
import           Data.Time
import           Import
import           Yesod.WebSockets

timeSource :: MonadIO m => ConduitT () TL.Text m ()
timeSource = forever $ do
    now <- liftIO getCurrentTime
    yield $ TL.pack $ show now
    liftIO $ threadDelay 5000000

getUnityTestR :: Handler Html
getUnityTestR = do
  (userId,_) <- requireAuthPair
  webSockets $ race_
    (runConduit $ sourceWS .|  Data.Conduit.List.map ((\s -> TL.pack (show userId) <> s) . TL.toUpper) .| sinkWSText)
    (runConduit $ timeSource .| sinkWSText)
  defaultLayout $ do
      setTitle "Unity Test"
      $(widgetFile "unityTest")
