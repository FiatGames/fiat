{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Queries.Game where

import           Control.Lens       (preview, view, _Wrapped')
import           Data.UUID          (UUID, toText)
import           Data.UUID.V4       (nextRandom)
import           Database.Esqueleto
import           DTO.GameDetail     (GameDetail (..))
import           DTO.Player         (Player (..))
import           FiatGame.Types
import           GameType
import           Import             hiding (delete, update, (=.), (==.))
import           Queries.Chat

updateGame :: Key Game -> SuccessfulProcessed -> DB ()
updateGame gameId success = get gameId >>= \case
  Nothing -> pure ()
  (Just game) -> do
    let gState = getGameStateMsg <$> view (successfulProccessedFromFiat.fromFiatGameState) success
        stage = view successfulProcessedGameStage success
        settings = getSettingsMsg $ view (successfulProccessedFromFiat.fromFiatSettings) success
        stateHash = fromMaybe (gameStateHash game) $ preview (successfulProccessedFromFiat.fromFiatGameHash._Wrapped') success
    update $ \g -> do
        set g
            [ GameSettings =. val settings
            , Import.GameState =. val gState
            , Import.GameStage =. val stage
            , GameStateHash =. val stateHash
            ]
        where_ (g ^. GameId ==. val gameId)

addUserToGame :: Key Game -> Key User -> SettingsMsg -> DB ()
addUserToGame gameId userId (SettingsMsg s) = do
  _ <- insert $ UserGame userId gameId
  addUserToGeneralChat userId gameId
  void $ update $ \g -> do
    set g [ GameSettings =. val s ]
    where_ (g ^. GameId ==. val gameId)

userGameQuery :: Key Game -> DB [Entity UserGame]
userGameQuery gameId = select $
    from $ \uG -> do
    where_ (uG ^. UserGameGameId ==. val gameId)
    pure uG

gameQuery :: Key User -> DB [Entity Game]
gameQuery userId = select $
    from $ \(uG,g) -> do
    where_ (uG ^. UserGameGameId ==. g ^. GameId &&. uG ^. UserGameUserId ==. val userId)
    pure g

isUserInGame :: Key Game -> Key User -> DB Bool
isUserInGame gameId userId = any ((==) gameId . entityKey) <$> gameQuery userId

gameByCodeQuery :: UUID -> DB [Entity Game]
gameByCodeQuery code = select $
    from $ \g -> do
    where_ (g ^. GameCode ==. val (toText code))
    pure g

insertNewGame :: Key User -> GameType -> Text -> DB (Key Game)
insertNewGame userId t n = do
    ng <- liftIO newGame
    gId <- insert ng
    _ <- insert $ UserGame userId gId
    cId <- insert $ ChatRoom "General" gId True
    _ <- insert $ ChatParticipants userId cId
    return gId
    where newGame = do
            code <- toText <$> nextRandom
            Just (FiatGameHash h,SettingsMsg ts) <- initialFromFiat t (FiatPlayer (fromSqlKey userId))
            pure (Game n t code SettingUp ts Nothing userId h)

getGameDetails :: Key Game -> DB GameDetail
getGameDetails gameId = do
    users <- select $
        from $ \(u, uG) -> do
        where_ (uG ^. UserGameGameId ==. val gameId &&. uG ^. UserGameUserId ==. u ^. UserId)
        pure u
    let pls = map (\(Entity k v) -> Player k (userIdent v)) users
    pure (GameDetail pls)
