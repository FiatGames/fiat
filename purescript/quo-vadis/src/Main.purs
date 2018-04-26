module Main where

import Prelude

import FiatGame.QuoVadis (Settings(..), ClientGameState(..))
import QuoVadis.Moves (Move(..))
import Components.Main as Main
import Config (getConfig)
import Control.Coroutine as CR
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (error)
import DOM (DOM)
import DOM.Websocket.WebSocket as WS
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import FiatGame.ToClient.Types as ToClient
import FiatGame.ToServer.Types as ToServer
import FiatGame.Types (FiatGameHash(..))
import FiatGame.WebSocket (wsConsumer, wsProducer, wsSender)
import FiatGame.Wrapper (QueryParams(..), makeWrapper)
import GameType (GameType(..))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Model (Game(..))

toQueryInput :: forall a. (ToClient.Msg Settings ClientGameState Move) -> a -> Main.Query a
toQueryInput enc = Main.HandleInput (Just enc)

fromQueryInput :: Main.Message -> (Tuple FiatGameHash (ToServer.Cmd Settings Move))
fromQueryInput msg = case msg of
    (Tuple h Main.StartGame) -> Tuple h ToServer.StartGame
    (Tuple h (Main.UpdateSettings s)) -> Tuple h $ ToServer.UpdateSettings s
    (Tuple h (Main.SubmitMove mv)) -> Tuple h $ ToServer.MakeMove mv

main :: Eff (HA.HalogenEffects (console :: CONSOLE, dom :: DOM)) Unit
main =  makeWrapper ["userId", "joinCode", "gameId"] >>= case _ of
    Left e -> error e
    Right (Tuple el params) -> case getConfig params of
        Left err -> error err
        Right config -> do
            connection <- WS.create (WS.URL (config.websocket <> "/game/" <> show (config.gameId))) []
            HA.runHalogenAff do
                body <- HA.awaitBody
                io <-runUI (Main.component config) Nothing el
                io.subscribe $ wsSender config.userId connection fromQueryInput
                CR.runProcess (wsProducer connection CR.$$ wsConsumer io.query toQueryInput)