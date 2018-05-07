module Components.TicTacToe where

import Prelude

import Components.Display as Display
import Components.Settings as Settings
import Data.Array (length)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Generic (gShow)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Tuple (Tuple(..))
import FiatGame.Types as FiatGame
import FiatGame.TicTacToe (GameState(..), Settings(..))
import FiatGame.ToClient.Types as ToClient
import FiatGame.ToServer.Types as ToServer
import FiatGame.Wrapper (QueryParams(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model (Game(..))
import TicTacToe.Types (Move, Spot)
import Config (Config)
type State = Maybe (ToClient.Msg Settings GameState Move)

type Input = State

data Query a 
  = HandleInput State a
  | HandleSettings Settings.Message a
  | HandleDisplay Display.Message a

data MsgCmd
  = StartGame
  | SpotChosen Spot

type Message = Tuple FiatGame.FiatGameHash MsgCmd

type ChildQuery = Coproduct2 Settings.Query Display.Query

type ChildSlot = Either2 Unit Unit

component :: forall m. Config -> H.Component HH.HTML Query Input Message m
component {joinCode, userId, api} =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where
  joinUri = api <> "/game/"<> joinCode <>"/join"
  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render Nothing = HH.div_ []
  render (Just (ToClient.Error _)) = HH.div_ []
  render (Just m@(ToClient.Msg {hash, settings:Settings s, state})) = HH.div_
    [ if showJoinLink
      then HH.p_
        [ HH.a [HP.href joinUri] [HH.text joinUri]
        ]
      else HH.div_ []
    , case state of
        Nothing -> HH.slot' CP.cp1 unit Settings.component (Settings s) (HE.input HandleSettings)
        Just _ -> HH.slot' CP.cp2 unit (Display.component userId) m (HE.input HandleDisplay)
    ]
    where
      showJoinLink = length s.players < 2 && (isNothing state || fromMaybe false (map showLinkForStage state))
      showLinkForStage (FiatGame.GameState{_gameStateStage:FiatGame.SettingUp}) = true
      showLinkForStage _ = false

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
  eval = case _ of
    HandleSettings Settings.StartGame next -> do
      H.get >>= case _ of
        (Just (ToClient.Msg {hash})) -> do
          H.raise $ Tuple hash StartGame
          pure next
        _ -> pure next
    HandleDisplay (Display.SpotChosen s) next -> do
      H.get >>= case _ of
        (Just (ToClient.Msg {hash})) -> do
          H.raise $ Tuple hash $ SpotChosen s
          pure next
        _ -> pure next
    HandleInput n next -> do
      oldN <- H.get
      H.put n
      pure next