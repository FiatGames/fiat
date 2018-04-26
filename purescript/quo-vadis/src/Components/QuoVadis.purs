module Components.QuoVadis where

import Prelude

import Components.Committee as Committee
import Components.Laurel as Laurel
import Data.Array as A
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Lens (_2, hasn't, over, traversed, view)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, maybe)
import Data.Traversable (maximum, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import FiatGame.QuoVadis (ClientGameState(..), Committee(..), Settings(..), _Committee)
import FiatGame.Types (FiatPlayer(..))
import FiatGame.Types as FiatGame
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import QuoVadis.Moves (Move(..), _CallVote, _MoveCaeser, _StartSenator)
import QuoVadis.Types (Edge(..), PlayerState(..), _PlayerState, psLaurels, psReserve)
import Svg.Attributes as SA
import Svg.Elements as SE

foreign import boardImg :: String

data InProgressMove 
  = MovingCaeser (Maybe Edge)
  | StartingSenator (Maybe Int)
  | CallingVote (Maybe { from :: Int, to :: Maybe Int})

moveReady :: InProgressMove -> Boolean
moveReady (MovingCaeser (Just _)) = true
moveReady (StartingSenator (Just _)) = true
moveReady (CallingVote (Just {to: Just _})) = true
moveReady _ = false

mkMove :: InProgressMove -> Maybe Move
mkMove (MovingCaeser (Just e)) = Just $ MoveCaeser e
mkMove (StartingSenator (Just n)) = Just $ StartSenator n
mkMove (CallingVote (Just {from: n1, to: Just n2})) = Just $ CallVote $ Edge { _eFrom: n1, _eTo: n2}
mkMove _ = Nothing

type Input = 
  { game :: FiatGame.GameState ClientGameState Move
  , settings :: Settings
  }

type State = 
  { input :: Input
  , inProgressMove :: Maybe InProgressMove 
  }

data Query a 
  = HandleInput Input a
  | HandleComittee Int Committee.Message a
  | HandleLaurel Edge Laurel.Message a
  | StartMoveClick InProgressMove a
  | SubmitMoveClick a

data Message 
  = SubmitMove Move

type ChildQuery = Coproduct2 Committee.Query Laurel.Query

type Slot = Either2 Int Edge

component :: forall m. Int -> H.Component HH.HTML Query Input Message m
component userId =
  H.parentComponent
    { initialState: \input -> {input, inProgressMove: Nothing }
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where
    render :: State -> H.ParentHTML Query ChildQuery Slot m
    render {input:{settings: Settings s, game: FiatGame.GameState{_gameStateState: ClientGameState gs}}, inProgressMove } = HH.div_ 
      [ HH.div [HP.class_ $ HH.ClassName "row"]
        [ HH.div 
          [ HP.class_ $ HH.ClassName "col-lg-9"] 
          [ SE.svg 
            [ SA.viewBox 0.0 0.0 200.0 165.0 ] $
            [ SE.element (HH.ElemName "image")
              [ SA.attr (HH.AttrName "href") boardImg
              , SA.height 165.0
              , SA.width 200.0
              , SA.x 0.0
              , SA.y 0.0
              ] []
            ] 
            <> (map (\t@(Tuple n c) -> HH.slot' CP.cp1 n (Committee.component n) {committee: currentCommittee t,selectable: selectableCommittee n} (HE.input $ HandleComittee n)) gs.cBoard)
            <> (map (\(Tuple e l) -> HH.slot' CP.cp2 e (Laurel.component e) {laurel: l, coveredWithCaeser: coveredWithCaeser e, selectable: selectableLaurel e, moveInProgress: laurelMoveInProgress e} (HE.input $ HandleLaurel e)) gs.cEdges)
          ]
        , HH.div 
            [ HP.class_ $ HH.ClassName "col-lg-3"] $
            map playerPanel (A.sortWith snd s.playerMap)
        ]
      ]
      where
        currentCommittee (Tuple n c@(Committee committee)) = case inProgressMove of
          Just (StartingSenator (Just n2)) -> if n == n2
            then Committee $ committee{ cPieces = addToCommittee (committee.cPieces) }
            else c
          _ -> c
        addToCommittee pcs = A.insert myPart $ A.deleteBy (\t1 t2 -> fst t1 == fst t2) (Tuple me []) pcs
          where
            maxInCommittee = fromMaybe (-1) $ maximum $ map (fromMaybe (-1) <<< maximum <<< snd) pcs
            myPart = over _2 (flip A.snoc $ maxInCommittee + 1) $ fromMaybe (Tuple me []) $ A.find ((==) me <<< fst) pcs
        selectableCommittee n = case inProgressMove of
          Just (StartingSenator Nothing) -> A.any canStartThere gs.cMoves
          Just (StartingSenator (Just n2)) -> n == n2
          Just (CallingVote Nothing) -> A.any canCallVoteFromThere gs.cMoves
          Just (CallingVote (Just {from: n2, to: Nothing})) -> n == n2 || A.any (canCallVoteToThere n2) gs.cMoves
          Just (CallingVote (Just {from: n2, to: Just n3})) -> n == n2 || n == n3
          _ -> false
          where 
            canCallVoteToThere n2 (CallVote (Edge {_eFrom,_eTo})) = _eFrom == n2 && _eTo == n
            canCallVoteToThere _ _ = false
            canCallVoteFromThere (CallVote (Edge {_eFrom})) = _eFrom == n
            canCallVoteFromThere _ = false
            canStartThere (StartSenator n2) = n == n2
            canStartThere _ = false          
        coveredWithCaeser e = case inProgressMove of
          Just (MovingCaeser mc) -> maybe (gs.cCaeser == e) (eq e) mc
          _ -> gs.cCaeser == e
        laurelMoveInProgress e = case inProgressMove of
          Just (MovingCaeser (Just e2)) -> e == e2
          _ -> false
        selectableLaurel e = case inProgressMove of
          Just (MovingCaeser mc) -> maybe (gs.cCaeser /= e) (eq e) mc
          _ -> false
        playerPanel t@(Tuple _ p) = if p == me then myPanel else otherPlayerPanel t        
        myPanel = HH.div 
          [ HP.class_ $ HH.ClassName $ "panel p"<> show me]
          [ HH.div
              [ HP.class_ $ HH.ClassName "panel-heading"]
              [ HH.text $ "Me - Player " <> show me]
          , HH.div
              [ HP.class_ $ HH.ClassName "panel-body"] $
              [ HH.dl_
                [ HH.dt 
                    [ HP.class_ $ HH.ClassName "rival"]
                    [ HH.text "Rival (click to reveal)"]
                , HH.dd 
                    [ HP.class_ $ HH.ClassName $ "rival p"<> show gs.cRival] 
                    [ HH.text $ "Player " <> show gs.cRival]
                , HH.dt_ [ HH.text "Senators in Reserve"]
                , HH.dd_ [ HH.text $ show $ view psReserve gs.cPlayerState]
                , HH.dt_ [ HH.text "Laurels"]
                , HH.dd 
                    [ HP.class_ $ HH.ClassName "laurels"] 
                    [ HH.text $ show $ view psLaurels gs.cPlayerState]
                ]
              ]
              <> if gs.cCurrentTurn /= me
                then 
                  []
                else 
                  [ HH.div 
                      [ HP.class_ $ HH.ClassName "alert alert-info"]
                      [ HH.text $ "My Turn" ]
                  , HH.div 
                      [ HP.class_ $ HH.ClassName "btn-group btn-group-xs"] 
                      [ HH.button 
                          [ HP.class_ $ HH.ClassName $ "btn btn-" <> if movingCaeser then "warning" else "primary"
                          , HP.disabled $ hasn't (traversed <<< _MoveCaeser) gs.cMoves
                          , HE.onClick $ HE.input_ (StartMoveClick $ MovingCaeser Nothing)
                          ] 
                          [ HH.text "Move Caeser"]
                      , HH.button 
                          [ HP.class_ $ HH.ClassName $"btn btn-" <> if callingVote then "warning" else "primary"
                          , HP.disabled $ hasn't (traversed <<< _CallVote) gs.cMoves
                          , HE.onClick $ HE.input_ (StartMoveClick $ CallingVote Nothing)
                          ] 
                          [ HH.text "Call Vote"]
                      , HH.button 
                          [ HP.class_ $ HH.ClassName $ "btn btn-" <> if startingSenator then "warning" else "primary"
                          , HP.disabled $ hasn't (traversed <<< _StartSenator) gs.cMoves
                          , HE.onClick $ HE.input_ (StartMoveClick $ StartingSenator Nothing)
                          ] 
                          [ HH.text "Start a Senator"]
                      ] 
                  , HH.div_
                      [ HH.button 
                        [ HP.class_ $ HH.ClassName "btn btn-success"
                        , HP.disabled $ not $ maybe false moveReady inProgressMove
                        , HE.onClick $ HE.input_ SubmitMoveClick
                        ] 
                        [ HH.text "Submit Move"]
                      ]
                  ]           
          ]
        otherPlayerPanel (Tuple fp p) = HH.div 
          [ HP.class_ $ HH.ClassName $ "panel p" <> show p]
          [ HH.div
              [ HP.class_ $ HH.ClassName "panel-heading"]
              [ HH.text $ "Player "<> show p <> " - " <> show fp]
          , HH.div
              [ HP.class_ $ HH.ClassName "panel-body"] $
              if gs.cCurrentTurn == p
              then 
                [ HH.div [HP.class_ $ HH.ClassName "alert alert-info"]
                  [ HH.text $ "Current Turn" ]
                ]
              else 
                []
          ]
        me = unsafePartial $ fromJust $ map snd $ A.find (f <<< fst) s.playerMap
          where 
            f (FiatPlayer p) = p == userId
            f _ = false
        movingCaeser = case inProgressMove of
          Just (MovingCaeser _) -> true
          _ -> false
        callingVote = case inProgressMove of
          Just (CallingVote _) -> true
          _ -> false
        startingSenator = case inProgressMove of
          Just (StartingSenator _) -> true
          _ -> false

    eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Message m
    eval = case _ of
      HandleInput i next -> do
        H.modify _{input = i}
        pure next
      HandleComittee n Committee.Selected next -> do
        { inProgressMove } <- H.get
        case inProgressMove of
          Just (StartingSenator mc) -> do
            H.modify _{ inProgressMove= Just $ StartingSenator $ maybe (Just n) (const Nothing) mc }
            pure next
          Just (CallingVote Nothing) -> do
            H.modify _{ inProgressMove= Just $ CallingVote $ Just {from: n, to: Nothing}}
            pure next
          Just (CallingVote (Just {from: n2, to: Nothing})) -> do
            when (n2 == n) do
              H.modify _{ inProgressMove= Just $ CallingVote Nothing}
              void $ H.query' CP.cp1 n (H.action Committee.Reset)
            when (n2 /= n) $ H.modify _{ inProgressMove= Just $ CallingVote $ Just {from: n2, to: Just n}}
            pure next
          Just (CallingVote (Just {from: n2, to:Just n3})) -> do
            when (n2 == n) do
              H.modify _{ inProgressMove= Just $ CallingVote Nothing}
              void $ H.query' CP.cp1 n (H.action Committee.Reset)
              void $ H.query' CP.cp1 n3 (H.action Committee.Reset)
            when (n3 == n) do
              H.modify _{ inProgressMove= Just $ CallingVote $ Just {from: n2, to: Nothing}}
              void $ H.query' CP.cp1 n (H.action Committee.Reset)
            pure next
          _ -> pure next
      HandleLaurel e Laurel.Selected next -> do
        { inProgressMove } <- H.get
        case inProgressMove of
          Just (MovingCaeser mc) -> do
            H.modify _{ inProgressMove= Just $ MovingCaeser $ maybe (Just e) (const Nothing) mc }
            pure next
          _-> pure next
      StartMoveClick mv next -> do
        resetAllComponents
        H.modify _{ inProgressMove= Just mv }
        pure next
      SubmitMoveClick next -> do
        {inProgressMove} <- H.get
        when (maybe false moveReady inProgressMove) do
          resetAllComponents
          H.modify _{ inProgressMove= Nothing }
          H.raise $ SubmitMove $ unsafePartial $ fromJust $ join $ map mkMove inProgressMove
        pure next

    resetAllComponents = do
      {input: {game: FiatGame.GameState{_gameStateState: ClientGameState gs}}} <- H.get
      traverse_ (\(Tuple n l) -> H.query' CP.cp1 n (H.action Committee.Reset)) (gs.cBoard)      
      traverse_ (\(Tuple e l) -> H.query' CP.cp2 e (H.action Laurel.Reset)) (gs.cEdges)
      