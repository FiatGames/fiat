{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module FutureMoveProcessor where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Lens
import qualified Data.Conduit.List
import           Data.Conduit.TMChan
import qualified Data.HashMap.Strict    as HM
import           Data.Time.Clock
import           Database.Persist.Sql   (fromSqlKey, runSqlPool, toSqlKey)
import           FiatChannel
import           FiatGame.Types
import           GameChannelProcessor
import           Import
import           Queries.FutureGameMove

reSyncFutureMoves :: App -> IO ()
reSyncFutureMoves app = do
    mvs <- runSqlPool allFutureGameMoves (appConnPool app)
    let mkMove (Entity _ f) = (fromSqlKey $ futureGameMoveGameId f, (futureGameMoveTime f,futureGameMoveMove f^._Unwrapped'))
    atomically $ modifyTVar' (appFutureMoves app) $ HM.union $ HM.fromList $ map mkMove mvs

processFutureMoves :: App -> IO ()
processFutureMoves app = do
    t <- getCurrentTime
    mvs <- HM.filter ((>=) t . fst) <$> readTVarIO (appFutureMoves app)
    _ <- forkIO $ mapConcurrently_ (flip runSqlPool (appConnPool app) . processMv) $ HM.toList mvs
    threadDelay $ appFutureMoveTimeout app
    processFutureMoves app
    where
        processMv (gameId,(_,ToServerMsg msg)) = do
            let gameKey = toSqlKey gameId
            processed <- processToServerMsg System gameKey app msg
            -- If someone beats you to making this future move it's okay
            if processed^.processedGameStateOutOfDate
            then pure ()
            else withChannel (appGameChannels app) gameKey
                $ \gameChan -> runConduit $ Data.Conduit.List.sourceList [processed^.processedToClientMsg] .| sinkTMChan gameChan
