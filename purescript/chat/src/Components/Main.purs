module Components.Main where

import Prelude

import Config (Config)
import Control.Monad.Aff (Aff)
import DTO.ChatCreate (ChatCreate(..))
import DTO.ChatMessageDetail (ChatMessageDetail(..))
import DTO.ChatRoomDetail (ChatRoomDetail(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Array as A
import Data.Either (Either(..))
import Data.Either.Nested (Either1)
import Data.Functor.Coproduct.Nested (Coproduct1)
import Data.Identity (Identity(..))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model (ChatRoom(..))
import Network.HTTP.Affjax (AJAX, AffjaxResponse, get, post)
import Network.HTTP.StatusCode (StatusCode(..))

type State =
  { msgs :: Map Int (Map Int ChatMessageDetail)
  , activeChat :: Maybe Int
  }

type Input = Array ChatMessageDetail

data Query a 
  = HandleInput Input a
  | HandleClicked a
  | NewRoom a
  | HandleRead Int a
  | HandleChangeChat Int a

data Message = Msg | Read Int

type ChildQuery = Coproduct1 Identity

type ChildSlot = Either1 Unit

component :: forall e. Config -> H.Component HH.HTML Query Input Message (Aff (ajax :: AJAX | e))
component {api, gameId} =
  H.parentComponent
    { initialState: \msgs -> { activeChat: Nothing, msgs: map (M.fromFoldable <<< map byMsgId) $ M.fromFoldableWith append $ map byRoomId msgs }
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  byRoomId cm@(ChatMessageDetail {room: (ChatRoomDetail {id, name})}) = Tuple id [cm]
  byMsgId cm@(ChatMessageDetail {messageId}) = Tuple messageId cm
  
  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (ajax :: AJAX | e))
  render {activeChat, msgs} = HH.div_ 
    [ HH.button [HE.onClick (HE.input_ HandleClicked)] [HH.text "click"]
    , HH.div 
      [ HP.classes [HH.ClassName "row"]] 
      [ HH.div 
        [ HP.classes [HH.ClassName "col-sm-3"]]
        [ HH.a 
            [ HE.onClick (HE.input_ NewRoom)] 
            [ HH.text "New Room"]
        , HH.ul 
          [ HP.classes 
            [ HH.ClassName "list-group"] 
            , HP.prop (H.PropName "style") "min-height:500px; max-height:500px; overflow-y: scroll;"
          ] $ 
          rooms 
        ]
      , HH.div 
        [ HP.classes [HH.ClassName "col-sm-9"]]
        [ HH.div 
            [ HP.classes [HH.ClassName "panel panel-default"]
            , HP.prop (H.PropName "style") "min-height:450px; max-height:450px; overflow-y: scroll;"
            ]
            [ HH.div 
                [ HP.classes [HH.ClassName "panel-body"]] $
                maybe [] (map msgLi <<< A.fromFoldable <<< M.values) $ activeChat >>= flip M.lookup msgs
            ]
        ]
      ]
    ]
    where
      rooms = A.fromFoldable $ M.values $ map roomLi msgs
      roomLi msgs = case L.head (M.values msgs) of
        Nothing -> HH.div_ []
        Just (ChatMessageDetail {room: (ChatRoomDetail {id, name})}) -> HH.li 
          [ HE.onClick (HE.input_ (HandleChangeChat id))
          , HP.classes [ HH.ClassName "list-group-item", HH.ClassName $ if activeChat == Just id then "active" else ""]
          ] 
          [ HH.text name]
      msgLi m@(ChatMessageDetail {messageId}) = HH.li_ 
        [ HH.text $ show m
        , HH.button [HE.onClick (HE.input_ (HandleRead messageId))] [HH.text "click"]
        ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Aff (ajax :: AJAX| e))
  eval = case _ of
    HandleChangeChat i next -> do
      H.modify _{activeChat= Just i}
      pure next
    HandleRead i next -> do
      H.raise $ Read i
      pure next
    HandleClicked next -> do
      resp :: AffjaxResponse Json <- H.liftAff $ get (api <> "/game/" <> show gameId)
      H.raise Msg
      pure next
    HandleInput ns next -> do
      {msgs} <- H.get
      let 
        byRoom msg@(ChatMessageDetail {room: (ChatRoomDetail {id}), messageId}) m = M.alter byMsg id m
          where byMsg Nothing = Just $ M.singleton messageId msg
                byMsg (Just m') = Just $ M.alter (const $ Just msg) messageId m'
      H.modify _{msgs= A.foldr byRoom msgs ns }
      pure next
    NewRoom next -> do
      resp :: AffjaxResponse Json <- H.liftAff $ post (api <> "/chat") $ encodeJson $ ChatCreate 
        { gameId : gameId
        , users : [2]
        , name : "Test"
        , initialMsg : ""
        }
      case resp.status of
        StatusCode 200 -> do
          case decodeJson (resp.response) of
            Left _ -> pure next
            Right (ChatRoomDetail {id}) -> do
              H.modify _{activeChat= Just id}
              pure next
        _ -> pure next