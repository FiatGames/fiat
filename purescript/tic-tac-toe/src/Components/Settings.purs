module Components.Settings where

import Prelude

import Data.Array (length)
import Data.Generic (gShow)
import FiatGame.TicTacToe (Settings(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input = State

type State = Settings

data Query a 
  = HandleInput State a
  | StartGameClick a

data Message = StartGame

component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  render :: State -> H.ComponentHTML Query
  render (Settings s)
    | length s.players < 2 = HH.div
      [ HP.classes [H.ClassName "alert alert-warning" ]]
      [ HH.text "Waiting on other player to join..." ]
    | otherwise = HH.div_ [  HH.button [HE.onClick (HE.input_ StartGameClick)] [HH.text "Start Game"]]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    HandleInput n next -> do
      oldN <- H.get
      H.put n
      pure next
    StartGameClick next -> do
      H.raise StartGame
      pure next