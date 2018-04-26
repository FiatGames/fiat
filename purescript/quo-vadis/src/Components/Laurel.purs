module Components.Laurel where

import Prelude

import Data.Array as A
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial, unsafePartialBecause)
import QuoVadis.Types (Laurel(..), Edge(..))
import Svg.Attributes as SA
import Svg.Elements as SE

foreign import caeserTokenImg :: String
foreign import caeser2Img :: String
foreign import tokenImgs :: Array String

type Input = 
  { laurel :: Maybe Laurel
  , coveredWithCaeser :: Boolean
  , selectable :: Boolean 
  , moveInProgress :: Boolean
  }

type State = 
  { input :: Input 
  , selected ::  Boolean  
  , showUnderneath :: Boolean
  }

data Query a 
  = HandleInput Input a
  | Select a
  | ToggleUnderneath a
  | Reset a

data Message 
  = Selected

component :: forall m. Edge -> H.Component HH.HTML Query Input Message m
component e = H.component
  { initialState: \input  -> { input, selected: false, showUnderneath: false}
  , render
  , eval
  , receiver: HE.input HandleInput
  }
  where
    render :: State -> H.ComponentHTML Query
    render state@{input : {laurel, coveredWithCaeser, selectable, moveInProgress}, selected, showUnderneath } 
      = case laurelCoord e of
        Nothing -> SE.g [] []
        Just (c@{x,y}) -> SE.g 
          []  
          if showUnderneath && coveredWithCaeser
          then imgWithCircle c (maybe "" laurelImg laurel) ToggleUnderneath
            <> imgWithCircle {x: x+(imgRadius * 2.0), y} caeserTokenImg ToggleUnderneath
          else 
            if coveredWithCaeser 
            then imgWithCircle c caeserTokenImg (if moveInProgress then Select else ToggleUnderneath)
            else imgWithCircle c (maybe "" laurelImg laurel) Select
      where 
        imgWithCircle {x, y} url onClick = 
          [ SE.element (HH.ElemName "image")
              [ SA.attr (HH.AttrName "href") url
              , SA.height (imgRadius * 2.0)
              , SA.width (imgRadius * 2.0)
              , SA.x (x - imgRadius)
              , SA.y (y - imgRadius)
              ] []
            , SE.circle 
              [ SA.cx x
              , SA.cy y
              , SA.r imgRadius
              , SA.attr (HH.AttrName "class") circleClass
              , HE.onClick (HE.input_ onClick)
              ]
            ]
        circleClass = joinWith " "  $ A.concat $
          [ ["laurel"] 
          , if (selected || moveInProgress) then ["selected"] else []
          , if selectable then ["selectable"] else []
          , if selectable || coveredWithCaeser then ["hoverable"] else []
          ]
        imgRadius = 6.6
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      ToggleUnderneath next -> do
        H.modify $ \s -> s{ showUnderneath = not s.showUnderneath}
        pure next
      Select next -> do
        {input:{selectable,moveInProgress}} <- H.get
        when (selectable || moveInProgress) $ do
          H.modify $ \s -> s{ selected = not s.selected}
          H.raise Selected
        pure next
      HandleInput i next -> do
        H.modify $ \s -> s{ input = i, selected= if not i.selectable then false else s.selected }
        pure next
      Reset next -> do
        H.modify $ \s -> s{ selected = false, showUnderneath = false}
        pure next

laurelCoord :: Edge -> Maybe {x::Number,y::Number}
laurelCoord = case _ of
  (Edge { _eFrom: 1, _eTo: 8})   -> Just {x: 45.860107, y: 107.4936}
  (Edge { _eFrom: 1, _eTo: 5})   -> Just {x: 65.293945, y: 121.4083}
  (Edge { _eFrom: 3, _eTo: 6})   -> Just {x: 163.04778, y: 120.9406}
  (Edge { _eFrom: 3, _eTo: 7})   -> Just {x: 180.25993, y: 120.8470}
  (Edge { _eFrom: 6, _eTo: 9})   -> Just {x: 132.55232, y: 84.64541}
  (Edge { _eFrom: 6, _eTo: 11})  -> Just {x: 145.36789, y: 79.03275}
  (Edge { _eFrom: 8, _eTo: 12})  -> Just {x: 20.273699, y: 59.39818}
  (Edge { _eFrom: 9, _eTo: 13})  -> Just {x: 99.998894, y: 47.69540}
  (Edge { _eFrom: 14, _eTo: 13}) -> Just {x: 145.46144, y: 30.9509}
  (Edge { _eFrom: 12, _eTo: 13}) -> Just {x: 54.162167, y: 31.138056}
  _ -> Nothing

laurelImg :: Laurel -> String
laurelImg (Laurel {_lScore, _lIsCaeser})
  = if _lIsCaeser
    then caeser2Img
    else unsafePartial $ A.unsafeIndex tokenImgs (_lScore - 1)
