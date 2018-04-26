module Components.Settings where

import Prelude

import Data.Array (length)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Number (fromString)
import Data.StrMap (update)
import FiatGame.QuoVadis (Settings(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP

type Input = {settings :: Settings, joinUri :: String}

type State = {settings :: Settings, joinUri :: String, updating :: Maybe Boolean}

data Query a 
  = HandleInput Input a
  | StartGameClick a
  | UpdateSettingsClick a
  | UpdateTurnTime String a
  | DismissAlert a

data Message 
  = StartGame
  | UpdateSettings Settings

component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: \s -> {settings: s.settings, joinUri: s.joinUri, updating: Nothing}
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  render :: State -> H.ComponentHTML Query
  render {settings: Settings s, joinUri, updating} = HH.div_ $
    [ HH.h2_ [HH.text "Quo Vadis Settings"]
    , HH.p_
      [ HH.a [HP.href joinUri] [HH.text joinUri] ] ] <> 
    maybe [] (if _ then [] else [ HH.div 
      [ HP.class_ (HH.ClassName "alert alert-success") ]
      [ HH.button 
        [ HP.class_ (HH.ClassName "close")
        , HE.onClick (HE.input_ DismissAlert)
        ] 
        [HH.text "✖"]
      , HH.text "Settings have been updated"
      ]
    ]) updating <> 
    [ HH.p_ 
      [ HH.text "People in the game"
      , HH.ul_ $ map (\p -> HH.li_ [HH.text $ show p]) s.players
      ]
    , HH.div [ HP.class_ (HH.ClassName "form")] 
      [ HH.div [ HP.class_ (HH.ClassName "form-group")] 
        [ HH.label [ HP.for "turnTime"]
          [ HH.text "Time for each turn (hours)"]
        , HH.input 
          [ HP.class_ (HH.ClassName "form-control")
          , HP.name "turnTime"
          , HP.type_ HP.InputNumber
          , HP.value $ show $ toNumber s.turnTimeMilliseconds / (milisecondCalc)
          , HE.onValueChange (HE.input UpdateTurnTime)
          , HP.step (HP.Step 0.1)
          ]
        ]
      , HH.button 
        [ HP.class_ (HH.ClassName "btn btn-primary")
        , HE.onClick (HE.input_ UpdateSettingsClick)
        ] 
        [ HH.text "Update Settings"]
      ]
    , HH.p_
      [ HH.button 
        [ HP.class_ (HH.ClassName "btn btn-danger")
        , HE.onClick (HE.input_ StartGameClick)
        , HP.disabled (length s.players < 3)
        ] 
        [ HH.text "Start Game"]
      ]
    ]
  milisecondCalc = 1000.0 * 60.0 * 60.0
  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    HandleInput n next -> do
      {updating} <- H.get
      H.modify _{settings= n.settings, joinUri= n.joinUri, updating= map (const false) updating}
      pure next
    StartGameClick next -> do
      H.raise StartGame
      pure next
    UpdateTurnTime t next -> do
      {settings:(Settings s)} <- H.get
      H.modify _{ settings= Settings s{turnTimeMilliseconds= round $ maybe 0.0 ((*) milisecondCalc) $ fromString t }}
      pure next
    DismissAlert next -> do
      H.modify _{ updating= Nothing}
      pure next
    UpdateSettingsClick next -> do
      {settings} <- H.get
      H.modify _{updating= Just true}
      H.raise $ UpdateSettings settings
      pure next