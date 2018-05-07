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

module FiatGame.HttpClient where

import           ClassyPrelude.Yesod                        hiding (get, pack,
                                                             responseBody,
                                                             responseStatus,
                                                             statusCode)
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy                       as BL
import           Data.Either
import           Data.Maybe
import qualified FiatGame.HttpRequestTypes.AddPlayerRequest as AddPlayerRequest
import qualified FiatGame.HttpRequestTypes.MakeMoveRequest  as MakeMoveRequest
import           FiatGame.Types
import           Network.Wreq

data HttpClientException = BadJSON String
  deriving (Show)

instance Exception HttpClientException

type ClientBaseUrl = String

mkRequest :: (FromJSON b, MonadIO m) => m (Response BL.ByteString) -> m b
mkRequest f = f >>= either (throwIO . BadJSON) pure . eitherDecode . view responseBody

defaultSettings :: ClientBaseUrl -> IO Value
defaultSettings baseUrl = mkRequest $ get (baseUrl ++ "/defaultSettings")

addPlayer :: ClientBaseUrl -> FiatPlayer -> Value -> IO (Maybe Value)
addPlayer baseUrl fp settings = mkRequest $ post (baseUrl ++ "/addPlayer") $ toJSON $ AddPlayerRequest.AddPlayerRequest fp settings

initialGameState :: ClientBaseUrl -> Value -> IO (Either Text (Value, GameState Value Value))
initialGameState baseUrl settings = mkRequest $ post (baseUrl ++ "/initialGameState") settings

makeMove :: ClientBaseUrl -> FiatPlayer -> Value -> GameState Value Value -> Value -> IO ( GameState Value Value)
makeMove baseUrl fp settings gs mv = mkRequest $ post (baseUrl ++ "/makeMove") $ toJSON $ MakeMoveRequest.MakeMoveRequest fp settings (Just gs) (Just mv)

isPlayersTurn :: ClientBaseUrl -> FiatPlayer -> Value -> GameState Value Value -> Value -> IO Bool
isPlayersTurn baseUrl fp settings gs mv = mkRequest $ post (baseUrl ++ "/isPlayersTurn") $ toJSON $ MakeMoveRequest.MakeMoveRequest fp settings (Just gs) (Just mv)

isMoveValid :: ClientBaseUrl -> FiatPlayer -> Value -> GameState Value Value -> Value -> IO Bool
isMoveValid baseUrl fp settings gs mv = mkRequest $ post (baseUrl ++ "/isMoveValid") $ toJSON $ MakeMoveRequest.MakeMoveRequest fp settings (Just gs) (Just mv)

toClientSettingsAndState :: ClientBaseUrl -> FiatPlayer -> Value -> Maybe (GameState Value Value) -> IO (Value, Maybe (GameState Value Value))
toClientSettingsAndState baseUrl fp settings gs = mkRequest $ post (baseUrl ++ "/toClientSettingsAndState") $ toJSON $ MakeMoveRequest.MakeMoveRequest fp settings gs Nothing

foo = do
  s <- defaultSettings "http://localhost:61307/"
  s <- fromJust <$> addPlayer "http://localhost:61307/" (FiatPlayer 1) s
  s <- fromJust <$> addPlayer "http://localhost:61307/" (FiatPlayer 2) s
  (Right (s,gs)) <- initialGameState "http://localhost:61307/" s
  gs <- makeMove "http://localhost:61307/" (FiatPlayer 1) s gs $ fromJust $ decode "{\"type\":\"PlaceMove\",\"spot\":\"UL\"}"
  gs <- makeMove "http://localhost:61307/" (FiatPlayer 2) s gs $ fromJust $ decode "{\"type\":\"PlaceMove\",\"spot\":\"UM\"}"
  gs <- makeMove "http://localhost:61307/" (FiatPlayer 1) s gs $ fromJust $ decode "{\"type\":\"PlaceMove\",\"spot\":\"ML\"}"
  gs <- makeMove "http://localhost:61307/" (FiatPlayer 2) s gs $ fromJust $ decode "{\"type\":\"PlaceMove\",\"spot\":\"MM\"}"
  makeMove "http://localhost:61307/" (FiatPlayer 2) s gs $ fromJust $ decode "{\"type\":\"PlaceMove\",\"spot\":\"LL\"}"

foo2 = do
  s <- defaultSettings "http://localhost:61307/"
  s <- fromJust <$> addPlayer "http://localhost:61307/" (FiatPlayer 1) s
  initialGameState "http://localhost:61307/" s

foo3 = do
  s <- defaultSettings "http://localhost:61307/"
  s <- fromJust <$> addPlayer "http://localhost:61307/" (FiatPlayer 1) s
  s <- fromJust <$> addPlayer "http://localhost:61307/" (FiatPlayer 2) s
  (Right (s,gs)) <- initialGameState "http://localhost:61307/" s
  t1 <- isPlayersTurn "http://localhost:61307/" (FiatPlayer 1) s gs $ fromJust $ decode "{\"type\":\"PlaceMove\",\"spot\":\"UL\"}"
  t2 <- isPlayersTurn "http://localhost:61307/" (FiatPlayer 2) s gs $ fromJust $ decode "{\"type\":\"PlaceMove\",\"spot\":\"UL\"}"
  t3 <- isMoveValid "http://localhost:61307/" (FiatPlayer 1) s gs $ fromJust $ decode "{\"type\":\"PlaceMove\",\"spot\":\"UL\"}"
  t4 <- toClientSettingsAndState "http://localhost:61307/" (FiatPlayer 1) s (Just gs)
  pure (t1,t2,t3,t4)

foo4 = do
  s <- defaultSettings "http://localhost:61307/"
  s <- fromJust <$> addPlayer "http://localhost:61307/" (FiatPlayer 1) s
  s <- fromJust <$> addPlayer "http://localhost:61307/" (FiatPlayer 2) s
  addPlayer "http://localhost:61307/" (FiatPlayer 3) s

foo5 = do
  s <- defaultSettings "http://localhost:61307/"
  s <- fromJust <$> addPlayer "http://localhost:61307/" (FiatPlayer 1) s
  toClientSettingsAndState "http://localhost:61307/" (FiatPlayer 1) s Nothing
