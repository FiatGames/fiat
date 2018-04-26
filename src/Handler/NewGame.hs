{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.NewGame where

import           GameType
import           Import
import           Queries.Game
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..),
                                        renderBootstrap3)

data NewGameForm = NewGameForm
  { newGameName :: Text
  , newGameType :: GameType
  } deriving Show

newGameForm :: Form NewGameForm
newGameForm = renderBootstrap3 BootstrapBasicForm $ NewGameForm
  <$> areq textField nameSettings Nothing
  <*> areq (selectFieldList games) typeSettings Nothing
  where
    games :: [(Text,GameType)]
    games = map (\g -> (gameTypeName g, g)) $ enumFrom QuoVadis
    nameSettings = FieldSettings
            { fsLabel = "Game Name"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                ]
            }
    typeSettings = FieldSettings
            { fsLabel = "Game Type"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                ]
            }

postNewGameR :: Handler Html
postNewGameR = do
  (userId,_) <- requireAuthPair
  ((result, widget), enctype) <- runFormPost newGameForm
  case result of
    FormSuccess game -> do
      gameId <- runDB $ insertNewGame userId (newGameType game) (newGameName game)
      redirect $ GamesR (GameR gameId)
    _ -> defaultLayout $ do
      setTitle "New Game"
      $(widgetFile "newGame")

getNewGameR :: Handler Html
getNewGameR = do
    (widget, enctype) <- generateFormPost newGameForm
    defaultLayout $ do
        setTitle "New Game"
        $(widgetFile "newGame")
