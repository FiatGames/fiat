module Components.Committee where

import Prelude

import Data.Array as A
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (maybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), fst, snd)
import FiatGame.QuoVadis (Committee(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Svg.Attributes as SA
import Svg.Elements as SE

type CommitteCoords = 
  { coords :: String
  , spots :: Array {x :: Number, y :: Number } 
  }

type Input = 
  { committee :: Committee
  , selectable :: Boolean
  }

type State = 
  { input :: Input
  , selected ::  Boolean 
  }

data Query a 
  = HandleInput Input a
  | Select a
  | Reset a

data Message
  = Selected

component :: forall m. Int -> H.Component HH.HTML Query Input Message m
component committeeNum = H.component
  { initialState: \input -> { input, selected: false}
  , render
  , eval
  , receiver: HE.input HandleInput
  }
  where
    render :: State -> H.ComponentHTML Query
    render {selected, input: {selectable,committee: (Committee committee)}} = SE.g [] $
      A.zipWith spotCircle committeeSpots c.spots <> 
      [ SE.path
          [ SA.attr (HH.AttrName "d") c.coords
          , SA.attr (HH.AttrName "class") committeeClass
          , HE.onClick (HE.input_ Select)
          ]
      ]
      where 
        committeeClass = joinWith " "  $ A.concat $
          [["committee"]
          , if selected then ["selected"] else []
          , if selectable then ["selectable"] else []
          , if selectable then ["hoverable"] else []
          ]
        c = committeeCoords committeeNum 
        spotCircle p {x,y} = SE.circle 
          [ SA.cx x
          , SA.cy y
          , SA.r 5.6
          , SA.attr (HH.AttrName "class") $ "committee-spot p" <> show p 
          ]
        committeeSpots = map fst 
          $ A.sortWith snd 
          $ A.concatMap (\(Tuple p pcs) -> map (Tuple p) pcs) committee.cPieces
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      Select next -> do
        {input:{selectable}} <- H.get
        when selectable $ do
          H.modify $ \s -> s{ selected = not s.selected}
          H.raise Selected
        pure next
      HandleInput i next -> do
        H.modify _{ input = i }
        pure next
      Reset next -> do
        H.modify _{ selected = false }
        pure next

spotCoords :: Partial => Int -> Array {x :: Number, y :: Number }
spotCoords = case _ of
  0  -> [{x: 21.298956,y: 148.49661 }]
  1  -> [{x: 52.718227,y: 137.2849 },{x: 52.982811,y: 148.46355 },{x: 66.542709,y: 148.3974 },{x: 80.433334,y: 148.52969 },{x: 80.168747,y: 137.74791 }]
  2  -> [{x: 133.94531,y: 148.33125 }]
  3  -> [{x: 167.48125,y: 147.80208 },{x: 180.77657,y: 147.73593 },{x: 180.51198,y: 136.82187 }]
  4  -> [{x: 21.034374,y: 113.07552 }]
  5  -> [{x: 65.748955,y: 105.93177 }]
  6  -> [{x: 133.68073,y: 116.71354 },{x: 133.35001,y: 105.4026 },{x: 146.71146,y: 105.4026 }]
  7  -> [{x: 180.71042,y: 105.5349 }]
  8  -> [{x: 20.637501,y: 77.092186 },{x: 33.866665,y: 77.092186 },{x: 47.49271,y: 77.026039 }]
  9  -> [{x: 87.114059,y: 66.178123 },{x: 87.114059,y: 77.158333 },{x: 100.67396,y: 77.356773 },{x: 113.83698,y: 77.092186 },{x: 114.10156,y: 65.979691 }]
  10 -> [{x: 180.51198,y: 74.975517 }]
  11 -> [{x: 145.58698,y: 53.941147 }]
  12 -> [{x: 21.232813,y: 42.497917 },{x: 21.166666,y: 31.319271 },{x: 34.528126,y: 31.451563 }]
  13 -> [{x: 79.044273,y: 26.159895 },{x: 87.973961,y: 18.486979 },{x: 100.47552,y: 16.304167 },{x: 112.58021,y: 18.15625 },{x: 121.70834,y: 25.63073 }]
  14 -> [{x: 166.81979,y: 32.972916 },{x: 179.78438,y: 32.840626 },{x: 179.85052,y: 43.688541 }]

committeeCoords :: Int -> CommitteCoords
committeeCoords = unsafePartial case _ of
  0  -> {spots: spotCoords 0, coords: "m 10.57051,139.18176 v 17.21216 h 21.141019 v -17.02507 z"}
  1  -> {spots: spotCoords 1, coords: "m 41.346596,128.04999 v 28.81165 h 49.578497 v -28.62457 z"}
  2  -> {spots: spotCoords 2, coords: "m 122.63662,138.99467 -0.18709,17.49279 h 21.79583 l -0.37417,-17.77342 z"}
  3  -> {spots: spotCoords 3, coords: "m 156.49967,138.71404 0.28063,17.58633 33.86305,-0.56126 -0.0935,-28.15685 h -20.95393 l 0.0935,11.13178 z"}
  4  -> {spots: spotCoords 4, coords: "m 9.7286108,103.72846 v 17.96051 H 31.52444 v -18.1476 z"}
  5  -> {spots: spotCoords 5, coords: "m 54.442801,96.338458 0.09355,17.773422 21.60874,0.0936 -0.467722,-17.960519 z"}
  6  -> {spots: spotCoords 6, coords: "m 122.50208,124.51875 -0.26458,-28.045829 33.86666,-0.529166 1.05834,17.462495 -13.22917,0.52917 0.26458,10.84792 z"}
  7  -> {spots: spotCoords 7, coords: "m 169.68942,96.151368 0.0935,17.866972 20.95393,-0.18709 V 96.244911 Z"}
  8  -> {spots: spotCoords 8, coords: "M 9.7286108,67.994524 V 85.580859 L 57.43622,85.39377 57.529766,67.620347 Z"}
  9  -> {spots: spotCoords 9, coords: "M 75.957998,56.862749 75.864454,85.39377 124.22687,85.019593 123.94624,57.049836 Z"}
  10 -> {spots: spotCoords 10, coords: "m 169.50233,65.375284 -0.0935,16.931523 21.51519,0.09354 -0.74835,-17.025067 z"}
  11 -> {spots: spotCoords 11, coords: "m 134.32966,44.421353 0.18709,17.492789 21.60874,0.18709 -0.56126,-17.399246 z"}
  12 -> {spots: spotCoords 12, coords: "M 10.476965,22.531978 10.102788,50.314645 H 31.243807 V 39.83768 l 13.283295,-0.280633 0.187089,-17.118613 z"}
  13 -> {spots: spotCoords 13, coords: "M 66.042298,5.9746311 65.574579,38.715148 134.6103,38.808691 133.86194,5.5069094 Z"}
  14 -> {spots: spotCoords 14, coords: "m 169.12815,51.624266 21.23457,0.374177 -0.46772,-27.876211 -34.61141,-0.467722 0.28064,17.025069 13.65747,0.467722 z"}