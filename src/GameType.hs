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
import qualified FiatGame.QuoVadis   as QuoVadis

data GameType = QuoVadis
  deriving (Show, Read, Eq, Generic, Enum, Ord)
derivePersistField "GameType"
$(deriveJSON defaultOptions ''GameType)

gameTypeName :: GameType -> Text
gameTypeName QuoVadis  = "Quo Vadis"

initialFromFiat QuoVadis = FiatGame.initialFromFiat (Data.Proxy.Proxy :: Data.Proxy.Proxy QuoVadis.Settings)

tryAddPlayer QuoVadis = FiatGame.tryAddPlayer (Data.Proxy.Proxy :: Data.Proxy.Proxy QuoVadis.Settings)

processToServer QuoVadis    = FiatGame.processToServer (Data.Proxy.Proxy :: Data.Proxy.Proxy QuoVadis.Settings)

toClientMsg QuoVadis    = FiatGame.toClientMsg (Data.Proxy.Proxy :: Data.Proxy.Proxy QuoVadis.Settings)

gameStateIsOutOfDate QuoVadis = FiatGame.gameStateIsOutOfDate (Data.Proxy.Proxy :: Data.Proxy.Proxy QuoVadis.Settings)

fromFiat QuoVadis = FiatGame.fromFiat (Data.Proxy.Proxy :: Data.Proxy.Proxy QuoVadis.Settings)
