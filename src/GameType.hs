{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module GameType where
import           ClassyPrelude.Yesod
import           Data.Aeson.TH
import           Data.Aeson.Types    (defaultOptions)
import qualified Data.Proxy
import qualified FiatGame.Class      as FiatGame
import qualified FiatGame.HttpClient as HttpClient
import qualified FiatGame.QuoVadis   as QuoVadis
import qualified FiatGame.TicTacToe  as TicTacToe
import qualified FiatGame.Types      as FiatGame

data GameType = QuoVadis | TicTacToe
  deriving (Show, Read, Eq, Generic, Enum, Ord)
derivePersistField "GameType"
$(deriveJSON defaultOptions ''GameType)

gameTypeName :: GameType -> Text
gameTypeName QuoVadis  = "Quo Vadis"
gameTypeName TicTacToe = "Tic-Tac-Toe"

initialFromFiat :: (MonadIO m) => GameType -> FiatGame.FiatPlayer -> m (Maybe (FiatGame.FiatGameHash, FiatGame.SettingsMsg))
initialFromFiat QuoVadis = flip runReaderT () . FiatGame.initialFromFiat (Data.Proxy.Proxy :: Data.Proxy.Proxy QuoVadis.Settings)
initialFromFiat TicTacToe = flip runReaderT "http://localhost:61307" . FiatGame.initialFromFiat (Data.Proxy.Proxy :: Data.Proxy.Proxy TicTacToe.Settings)

tryAddPlayer QuoVadis p = flip runReaderT () . FiatGame.tryAddPlayer (Data.Proxy.Proxy :: Data.Proxy.Proxy QuoVadis.Settings) p
tryAddPlayer TicTacToe p = flip runReaderT "http://localhost:61307" . FiatGame.tryAddPlayer (Data.Proxy.Proxy :: Data.Proxy.Proxy TicTacToe.Settings) p

processToServer QuoVadis  m f  = flip runReaderT () . FiatGame.processToServer (Data.Proxy.Proxy :: Data.Proxy.Proxy QuoVadis.Settings) m f
processToServer TicTacToe m f  = flip runReaderT "http://localhost:61307" . FiatGame.processToServer (Data.Proxy.Proxy :: Data.Proxy.Proxy TicTacToe.Settings) m f

toClientMsg QuoVadis    p = flip runReaderT () . FiatGame.toClientMsg (Data.Proxy.Proxy :: Data.Proxy.Proxy QuoVadis.Settings) p
toClientMsg TicTacToe    p = flip runReaderT "http://localhost:61307" . FiatGame.toClientMsg (Data.Proxy.Proxy :: Data.Proxy.Proxy TicTacToe.Settings) p

gameStateIsOutOfDate QuoVadis = FiatGame.gameStateIsOutOfDate (Data.Proxy.Proxy :: Data.Proxy.Proxy QuoVadis.Settings)
gameStateIsOutOfDate TicTacToe =  FiatGame.gameStateIsOutOfDate (Data.Proxy.Proxy :: Data.Proxy.Proxy TicTacToe.Settings)

fromFiat QuoVadis = FiatGame.fromFiat (Data.Proxy.Proxy :: Data.Proxy.Proxy QuoVadis.Settings)
fromFiat TicTacToe = FiatGame.fromFiat (Data.Proxy.Proxy :: Data.Proxy.Proxy TicTacToe.Settings)
