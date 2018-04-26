{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module FiatChannel where

import           ClassyPrelude.Yesod
import qualified Data.HashMap.Strict  as HM
import           Database.Persist.Sql (fromSqlKey)

type FiatChannel a = TVar (HashMap Int64 (Int, TMChan a))

withChannel :: (ToBackendKey SqlBackend record, MonadUnliftIO m) => FiatChannel b -> Key record -> (TMChan b -> m c) -> m c
withChannel c a = bracket (atomically $ getChan c $ fromSqlKey a) (const $ atomically $ dropFromChan c $ fromSqlKey a)

dropFromChan :: FiatChannel a -> Int64 -> STM ()
dropFromChan chanMap k = lookup k <$> readTVar chanMap >>= \case
    Nothing -> pure ()
    (Just (i,chan)) ->
        if i <= 1
        then do
            closeTMChan chan
            modifyTVar chanMap $ HM.delete k
        else modifyTVar chanMap $ HM.insert k (i-1,chan)

getChan :: FiatChannel a -> Int64 -> STM (TMChan a)
getChan moves k = do
    parChan <- lookup k <$> readTVar moves >>= \case
        Just (i,chan) -> do
            modifyTVar moves $ HM.insert k (i+1,chan)
            pure chan
        Nothing -> do
            chan <- newBroadcastTMChan
            modifyTVar moves $ HM.insert k (1,chan)
            pure chan
    dupTMChan parChan
