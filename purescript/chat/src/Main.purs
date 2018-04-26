module Main where

import Prelude

import Chat.Types as Chat
import Components.Main as Main
import Config (getConfig)
import Control.Coroutine as CR
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (error)
import DOM (DOM)
import DOM.Websocket.WebSocket as WS
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import FiatGame.WebSocket (wsChatConsumer, wsProducer, wsChatSender)
import FiatGame.Wrapper (makeWrapper)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)

toQueryInput :: forall a. Chat.ToClient -> a -> Main.Query a
toQueryInput (Chat.Messages {msgs}) = Main.HandleInput msgs

fromQueryInput :: Main.Message -> Chat.ToServer
fromQueryInput msg = case msg of
    Main.Msg -> Chat.PostMessage 1 "blah"
    Main.Read i -> Chat.ReadMessage i

main :: Eff (HA.HalogenEffects (console :: CONSOLE, dom :: DOM, ajax :: AJAX)) Unit
main = makeWrapper ["gameId", "userId"] >>= case _ of
    Left e -> error e
    Right (Tuple el params) -> case getConfig params of
        Left e -> error e
        Right config -> do
            let socketUri = config.websocket <> "/game/" <> show (config.gameId) <> "/chat"
            connection <- WS.create (WS.URL socketUri) []
            HA.runHalogenAff do
                body <- HA.awaitBody
                io <-runUI (Main.component config) [] el
                io.subscribe $ wsChatSender config.userId connection fromQueryInput
                CR.runProcess (wsProducer connection CR.$$ wsChatConsumer io.query toQueryInput)
