{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Game where

import           Control.Lens
import qualified Data.Conduit.List
import           Data.Conduit.TMChan
import qualified Data.Map             as M
import           Data.Maybe
import           Data.UUID            (fromText)
import           Database.Persist.Sql (fromSqlKey)
import           FiatChannel
import           FiatGame.Types
import           GameChannelProcessor
import           GameType
import           Import
import           Queries.Game
import           Yesod.WebSockets     (sinkWSText, sourceWS, webSockets)

ensureInGame :: GameId -> UserId -> Handler Game
ensureInGame gameId userId = do
    game <- runDB $ get404 gameId
    games <- runDB $ gameQuery userId
    when (gameId `notElem` map entityKey games) $ permissionDenied "You are not in this game"
    pure game

initialMsg :: Key User -> Game -> ToFiatMsg
initialMsg userId g = fromFiat (gameType g) (FiatPlayer (fromSqlKey userId)) fiat
    where fiat = FromFiat (SettingsMsg $ gameSettings g) (GameStateMsg <$> gameState g) (FiatGameHash $ gameStateHash g)

getChatJavascriptPath :: Key User -> GameId -> WidgetFor App Text
getChatJavascriptPath userId gameId = do
    render <- getUrlRenderParams
    let queryParams =
            [ ("gameId", pack (show (fromSqlKey gameId)))
            , ("userId", pack (show (fromSqlKey userId)))
            ]
    pure $ render (StaticR (StaticRoute ["js", "chat.js"] [])) queryParams

getJavascriptPath :: Key User -> Game -> GameId -> WidgetFor App Text
getJavascriptPath userId (Game _ g code _ _ _ _ _) gameId = do
    render <- getUrlRenderParams
    let queryParams =
            [ ("gameId", pack (show (fromSqlKey gameId)))
            , ("userId", pack (show (fromSqlKey userId)))
            , ("joinCode", code)
            ]
        localJs file = render (StaticR (StaticRoute ["js", file] [])) queryParams
    case g of
        QuoVadis  -> pure $ localJs "quo-vadis.js"

getAllGamesR :: Handler Html
getAllGamesR = do
    (userId,_) <- requireAuthPair
    games <- runDB $ gameQuery userId
    let seperate :: GameStage -> M.Map GameType [Entity Game]
        seperate stage = M.fromList $ map (\e@(Entity _ g) -> (gameType g, [e])) $ filter ((stage ==) . gameStage . entityVal) games
        getGameTypeName :: [Entity Game] -> Text
        getGameTypeName = maybe "" (\(Entity _ g) -> gameTypeName $ gameType g) . headMay
        gamesDone = seperate Done
        gamesInProgress = seperate Playing
        gamesSettingUp = seperate SettingUp
    defaultLayout $ do
        setTitle "Game"
        $(widgetFile "games")


getJoinGameR :: Text -> Handler Html
getJoinGameR uuid = do
    (userId,_) <- requireAuthPair
    case fromText uuid of
        Nothing   -> invalidArgs ["Code is incorrect"]
        Just code -> do
            gs <- runDB $ gameByCodeQuery code
            case headMay gs of
                Nothing -> notFound
                Just g@(Entity gameId _) -> do
                    games <- runDB $ gameQuery userId
                    app <- getYesod
                    when (gameId `elem` map entityKey games) $ redirect $ GamesR (GameR gameId)
                    runDB (addUserToGameIfCan userId g (appGameLocks app)) >>= \case
                        True -> do
                            game <- runDB $ get404 gameId
                            withChannel (appGameChannels app) gameId $ \myChan ->
                                runConduit $ Data.Conduit.List.sourceList [game]
                                        .| Data.Conduit.List.map (initialMsg userId)
                                        .| sinkTMChan myChan
                            redirect $ GamesR (GameR gameId)
                        False -> defaultLayout $ do
                            setTitle "Cannot Join Game"
                            $(widgetFile "joinFailed")

getGameR :: GameId -> Handler TypedContent
getGameR gameId = do
    (userId,_) <- requireAuthPair
    game <- ensureInGame gameId userId
    app <- getYesod
    initMsg <- initialMsg userId <$> runDB (getJust gameId)
    webSockets $ withChannel (appGameChannels app) gameId $ \myChan -> do
            let fromChannel = liftIO . fmap getToClientMsg . toClientMsg (gameType game) (FiatPlayer (fromSqlKey userId))
                fromWebSocket = lift . fmap (view processedToClientMsg) . runDB . processToServerMsg (FiatPlayer (fromSqlKey userId)) gameId app
            runConduit $ Data.Conduit.List.sourceList [initMsg] .| Data.Conduit.List.mapM fromChannel .| sinkWSText
            race_
                (runConduit $ sourceWS .| Data.Conduit.List.mapM fromWebSocket .| sinkTMChan myChan)
                (runConduit $ sourceTMChan myChan .| Data.Conduit.List.mapM fromChannel .| sinkWSText)
    let
        html = do
            jsSrc <- getJavascriptPath userId game gameId
            chatJs <- getChatJavascriptPath userId gameId
            setTitle "Game"
            $(widgetFile "game")
        json = do
            details <- runDB $ getGameDetails gameId
            returnJson details
    defaultLayoutJson html json
