{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Queries.FutureGameMove where

import           Database.Esqueleto
import           FiatGame.Types
import           Import             hiding (delete, update, (=.), (==.))

futureGameMoveByGame :: Key Game -> DB [Entity FutureGameMove]
futureGameMoveByGame gameId = select $
    from $ \mv -> do
    where_ (mv ^. FutureGameMoveGameId ==. val gameId)
    pure mv

deleteFutureGameMove :: Key Game -> DB ()
deleteFutureGameMove gameId = delete $
    from $ \fmv -> do
    where_ (fmv ^. FutureGameMoveGameId ==. val gameId)

insertFutureGameMove :: Key Game -> (UTCTime,ToServerMsg) -> DB (Key FutureGameMove)
insertFutureGameMove gameId (t,ToServerMsg mv) = insert (FutureGameMove gameId t mv)

-- addOrUpdateFutureGameMove :: (MonadIO m) => Key Game -> UTCTime -> Text -> SqlPersistT m ()
-- addOrUpdateFutureGameMove gameId t mv = do
--     fmvs <- futureGameMoveByGame gameId
--     if null fmvs
--     then void insertIt
--     else updateIt
--     where
--         insertIt = insert (FutureGameMove gameId t mv)
--         updateIt = update $ \fmv -> do
--             set fmv [ FutureGameMoveTime =. val t, FutureGameMoveMove =. val mv ]
--             where_ (fmv ^. FutureGameMoveGameId ==. val gameId)

allFutureGameMoves :: DB [Entity FutureGameMove]
allFutureGameMoves = select $ from $ \fmvs -> pure fmvs
