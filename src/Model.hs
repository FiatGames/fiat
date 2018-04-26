{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import           ClassyPrelude.Yesod
import           Data.Aeson.TH
import           Data.Aeson.Types       (defaultOptions)
import           Database.Persist.Quasi
import           FiatGame.Instances     ()
import           FiatGame.Types         (GameStage (..))
import           GameType               (GameType (..))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

$(deriveJSON defaultOptions ''Game)
$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''ChatRoom)
$(deriveJSON defaultOptions ''ChatMessage)
