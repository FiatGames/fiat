{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module GameChannelProcessor where

import           Control.Concurrent.STM.Lock (Lock)
import qualified Control.Concurrent.STM.Lock as Lock
import           Control.Lens
import qualified Data.HashMap.Strict         as HM
import           Data.Maybe
import           Database.Persist.Sql        (fromSqlKey)
import           FiatGame.Types
import           GameType
import           Import
import           Queries.FutureGameMove
import           Queries.Game

withLock :: MonadUnliftIO m => Lock -> m b -> m b
withLock lock inner = withRunInIO $ \runInIO -> Lock.with lock $ runInIO inner

getGameStateLock :: GameLocks -> Key Game -> STM Lock
getGameStateLock locks gameId = lookup (fromSqlKey gameId) <$> readTVar locks >>= \case
    Just c -> pure c
    Nothing -> do
        lock <- Lock.new
        modifyTVar locks (HM.insert (fromSqlKey gameId) lock)
        pure lock

addUserToGameIfCan :: MonadUnliftIO m => Key User -> Entity Game -> GameLocks -> ReaderT SqlBackend m Bool
addUserToGameIfCan userId (Entity gameId game) locks = do
    lock <- atomically $ getGameStateLock locks gameId
    withLock lock $ do
        mpl <- tryAddPlayer (gameType game) (FiatPlayer (fromSqlKey userId)) (SettingsMsg (gameSettings game))
        case mpl of
            Nothing -> pure False
            Just s  -> addUserToGame gameId userId s >> pure True

processToServerMsg :: MonadUnliftIO m => FiatPlayer -> Key Game -> App -> Text -> ReaderT SqlBackend m Processed
processToServerMsg pl gameId app msg = do
    game <- get404 gameId
    lock <- atomically $ getGameStateLock (appGameLocks app) gameId
    withLock lock $ do
        _ <- deleteFutureGameMove gameId
        _ <- atomically $ modifyTVar' (appFutureMoves app) $ HM.delete (fromSqlKey gameId)
        let f = FromFiat (SettingsMsg $ gameSettings game) (GameStateMsg <$> gameState game) (FiatGameHash $ gameStateHash game)
            mv = MoveSubmittedBy pl
            toServerMsg = ToServerMsg msg
        processed <- processToServer (gameType game) mv f toServerMsg
        case processed^.processedSuccessFul of
            Nothing -> pure ()
            Just success -> do
                _ <- updateGame gameId success
                case success^?successfulProcessedFutureMove._Just of
                    Nothing -> pure ()
                    Just fmv -> do
                        _ <- insertFutureGameMove gameId fmv
                        atomically $ modifyTVar' (appFutureMoves app) $ HM.insert (fromSqlKey gameId) fmv
        pure processed
