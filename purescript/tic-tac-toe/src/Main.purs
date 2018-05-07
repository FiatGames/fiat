module Main where

import Prelude

import Components.TicTacToe as TicTacToe
import Config (getConfig)
import Control.Coroutine as CR
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (error)
import DOM (DOM)
import DOM.Websocket.WebSocket as WS
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import FiatGame.TicTacToe (GameState(..), Settings(..))
import FiatGame.ToClient.Types as ToClient
import FiatGame.ToServer.Types as ToServer
import FiatGame.Types (FiatGameHash(..))
import FiatGame.WebSocket (wsConsumer, wsConsumer', wsProducer, wsSender, wsSender')
import FiatGame.Wrapper (QueryParams(..), makeWrapper)
import GameType (GameType(..))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Model (Game(..))
import TicTacToe.Types (Move(..), Spot(..))

toQueryInput :: forall a. (ToClient.Msg Settings GameState Move) -> a -> TicTacToe.Query a
toQueryInput enc = TicTacToe.HandleInput $ Just enc

fromQueryInput :: TicTacToe.Message -> (Tuple FiatGameHash (ToServer.Cmd Settings Move))
fromQueryInput msg = case msg of
    (Tuple h TicTacToe.StartGame) -> Tuple h ToServer.StartGame
    (Tuple h (TicTacToe.SpotChosen s)) -> Tuple h (ToServer.MakeMove (Place s))

main :: Eff (HA.HalogenEffects (console :: CONSOLE, dom :: DOM)) Unit
main =  makeWrapper ["userId", "joinCode", "gameId"] >>= case _ of
    Left e -> error e
    Right (Tuple el params) -> case getConfig params of
        Left err -> error err
        Right config -> do
            connection <- WS.create (WS.URL (config.websocket <> "/game/" <> show (config.gameId))) []
            HA.runHalogenAff do
                body <- HA.awaitBody
                io <-runUI (TicTacToe.component config) Nothing el
                io.subscribe $ wsSender config.userId connection fromQueryInput
                CR.runProcess (wsProducer connection CR.$$ wsConsumer io.query toQueryInput)
