module Components.Main where

import Prelude

import Components.Settings as Settings
import Components.QuoVadis as QuoVadis
import Config (Config)
import Data.Array (length)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Generic (gShow)
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Tuple (Tuple(..))
import FiatGame.ToClient.Types as ToClient
import FiatGame.Types as FiatGame
import FiatGame.Wrapper (QueryParams(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import FiatGame.QuoVadis (Settings(..), ClientGameState(..))
import QuoVadis.Moves (Move(..))
import Data.Identity (Identity)

type State = Maybe (ToClient.Msg Settings ClientGameState Move)

type Input = State

data Query a 
  = HandleInput State a
  | HandleSettings Settings.Message a
  | HandleQuoVadis QuoVadis.Message a

data MsgCmd 
  = StartGame
  | UpdateSettings Settings
  | SubmitMove Move

type Message = Tuple FiatGame.FiatGameHash MsgCmd

type ChildQuery = Coproduct2 Settings.Query QuoVadis.Query

type ChildSlot = Either2 Unit Unit

component :: forall m. Config -> H.Component HH.HTML Query Input Message m
component {api, joinCode, userId} =
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
  render (Just (ToClient.Error{error})) = HH.div_ [ HH.h1_ [HH.text (gShow error)]]
  render (Just (ToClient.Msg{hash, settings: settings@(Settings s), state})) = HH.div_
    [ case state of
        Nothing
          -> HH.slot' CP.cp1 unit Settings.component {settings,joinUri} (HE.input $ HandleSettings)
        Just f
          -> HH.slot' CP.cp2 unit (QuoVadis.component userId) {game: f, settings} (HE.input $ HandleQuoVadis)
    ]
    
  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
  eval = case _ of
    HandleQuoVadis (QuoVadis.SubmitMove mv) next -> H.get >>= case _ of
      (Just (ToClient.Msg{hash})) -> do
        H.raise $ Tuple hash (SubmitMove mv)
        pure next
      _ -> pure next
    HandleSettings s next -> H.get >>= case _ of
      (Just (ToClient.Msg{hash})) -> case s of
        Settings.StartGame -> do
          H.raise $ Tuple hash StartGame
          pure next
        Settings.UpdateSettings s -> do
          H.raise $ Tuple hash (UpdateSettings s)
          pure next
      _ -> pure next
    HandleInput n next -> do
      oldN <- H.get
      H.put n
      pure next