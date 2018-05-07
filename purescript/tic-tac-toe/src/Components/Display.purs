module Components.Display where

import Prelude

import Components.Spot as Spot
import Data.Array (findIndex, index)
import Data.Either (Either(..))
import Data.Generic (gCompare, gEq, gShow)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Tuple (Tuple(..), snd)
import FiatGame.Types as FiatGame
import FiatGame.TicTacToe (GameState(..), Settings(..))
import FiatGame.ToClient.Types as ToClient
import FiatGame.ToServer.Types as ToServer
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model (Game(..))
import TicTacToe.Types (GameOver(..), Move, Player(..), Spot(..))

type Input = State

type State = ToClient.Msg Settings GameState Move

data Query a 
  = HandleInput Input a
  | HandleSpot Spot Spot.Message a

data Message 
  = SpotChosen Spot

newtype Slot = Slot Spot
instance showSlot :: Show Slot where
  show (Slot s1) = gShow s1
instance eqSlot :: Eq Slot where
  eq (Slot s1) (Slot s2) = gEq s1 s2
instance ordSlot :: Ord Slot where
  compare (Slot s1) (Slot s2) = gCompare s1 s2

component :: forall m. Int -> H.Component HH.HTML Query Input Message m
component me =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where
  
  render :: State -> H.ParentHTML Query Spot.Query Slot m
  render (ToClient.Error {error}) = HH.div_ [ HH.h1_ [HH.text (gShow error)]]
  render (ToClient.Msg {hash,settings, state:Nothing}) = HH.div_ [  HH.p_ [HH.text (gShow settings)]]  
  render (ToClient.Msg {hash,settings:Settings s, state:Just (FiatGame.GameState {_gameStateState: GameState gs})}) = HH.div
      [HP.classes [H.ClassName "container-fluid"]] $ (if isNothing gs.gameOver then currentTurnAlert else gameOver) <> gameBoard          
      where
        currentTurnAlert = 
          [ HH.div 
            [ HP.classes [H.ClassName $ "alert " <> if myTurn then " alert-success" else " alert-warning" ]]
            [ HH.text if myTurn then "Your turn!" else "Waiting on other player to move..." ]
          ]
        showPlayer X = "X"
        showPlayer O = "O"
        gameOver = case gs.gameOver of
            (Just (Win p)) -> 
              [ HH.h1 [HP.attr (HH.AttrName "style") "text-align:center;"] 
                [ if gEq p mePlayer 
                  then HH.text $ "You WIN!"
                  else HH.text $ "You LOSE!"
                ]
              ]
            (Just Draw) ->  [HH.h1 [HP.attr (HH.AttrName "style") "text-align:center;"] [HH.text "It's a DRAW"]]
            Nothing -> []
        gameBoard = 
          [ HH.div [HP.classes [H.ClassName "row"]]
            [ spot UL, spot UM, spot UR ]
          , HH.div [HP.classes [H.ClassName "row"]]
            [ spot ML, spot MM, spot MR]
          , HH.div [HP.classes [H.ClassName "row"]]
            [ spot DL, spot DM, spot DR]
          ]
        myTurn = isNothing gs.gameOver && gEq gs.turn mePlayer
        spot sp = HH.slot (Slot sp) Spot.component (getSpot sp) (HE.input $ HandleSpot sp)
        getSpot sp = 
          { player : join $ map snd $ findIndex (\(Tuple s _) -> (Slot s) == (Slot sp)) gs.board >>= index gs.board
          , canMakeMove : myTurn
          }
        mePlayer = if gEq (Just (FiatGame.FiatPlayer me)) s.xPlayer
                    then X
                    else O
  eval :: Query ~> H.ParentDSL State Query Spot.Query Slot Message m
  eval = case _ of
    HandleInput n next -> do
      oldN <- H.get
      H.put n
      pure next
    HandleSpot sp m next -> case m of
      Spot.Chosen -> do
        H.raise $ SpotChosen sp
        pure next