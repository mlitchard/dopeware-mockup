{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE JavaScriptFFI       #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE TypeFamilies        #-}

module DopeWars where

import Prelude hiding (lookup)

import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Map.Strict
-- import qualified Data.Text as T
import Data.Text hiding (map,empty)

import Reflex.Dom

import Schema
rowOne, rowTwo, rowThree :: [PlanetName]
rowOne   = [Arrakis, Minbar, Tatooine]
rowTwo   = [CentauriPrime, Vulcan, Dantooine]
rowThree = [Mongo, Terra, Pluto]


dopeWars :: forall m t .
                ( DomBuilder t m
                , PostBuild t m
                , MonadFix m
                , MonadHold t m
                , MonadIO m)
                => Event t (GameState) -> m ()
dopeWars initE = do
    _ <- mfix go
    return ()
    where
        go :: Event t GameState -> m (Event t GameState)
        go newStateE = do
            let stateE' = leftmost [newStateE, initE]
            divClass "section" $ do
                switchScreenE :: (Event t GameState) <-
                    screenSwitchingPanel stateE'
                let stateE = leftmost [switchScreenE]
                -- divClass "level" $ do
                    
                return stateE

screenSwitchingPanel :: forall m t .
                            ( DomBuilder t m
                            , PostBuild t m
                            , MonadFix m
                            , MonadHold t m
                            , MonadIO m) 
                            => Event t GameState -> m (Event t GameState)
screenSwitchingPanel gameStateE = do
    gameStateDyn <- holdDyn initGameState gameStateE
    let gsE :: Event t (m (Event t GameState))
        gsE = ffor (_screenState <$> gameStateE) $ \case
                      Travel -> travelBoard gameStateDyn
                      Market -> marketPanel gameStateDyn
    updatedGameStateDyn :: Dynamic t (Event t GameState) <- 
        widgetHold (return never) gsE
    let updatedGameStateE = traceEvent "switchPromptlyDyn" 
                                $ switchPromptlyDyn updatedGameStateDyn
    return updatedGameStateE

updateScreen :: GameState -> ScreenState -> GameState
updateScreen gState screenState = gState {_screenState = screenState}

travelBoard :: forall m t .
                   ( DomBuilder t m
                   , PostBuild t m
                   , MonadFix m
                   , MonadHold t m
                   , MonadIO m)
                   => Dynamic t GameState -> m (Event t GameState)
travelBoard gameStateDyn = do
    boardSwitchE <- divClass "level" $ do
        travelPanel gameStateDyn
    planetSwitchE <- divClass "level" $ do
        travelMap gameStateDyn
    return $ leftmost [boardSwitchE, planetSwitchE]

travelPanel :: forall m t .
                   ( DomBuilder t m
                   , PostBuild t m
                   , MonadFix m
                   , MonadHold t m
                   , MonadIO m)
                   => Dynamic t GameState -> m (Event t GameState)
travelPanel gameStateDyn = do
    divClass "level-left" $ do
        text "Click on a highlighted planet to travel there"
    updatedScreenE <- divClass "level-right" $ do
        (elt,_) <- elClass' "button" panelButton $ do
            text "go to market"
        return $ traceEvent "MarketClick" $ Market <$ domEvent Click elt 
    let gameStateB = current gameStateDyn
        updatedGameStateE = attachWith
                                updateScreen
                                gameStateB
                                updatedScreenE
    return updatedGameStateE

travelMap :: forall m t .
                 ( DomBuilder t m
                 , PostBuild t m
                 , MonadFix m
                 , MonadHold t m
                 , MonadIO m)
                 => Dynamic t GameState -> m (Event t GameState)
travelMap gameStateDyn = do
    divClass "section" $ do
        rowOneE :: Event t GameState <-
            divClass "level" $ do
                leftmost <$> mapM (planetButton gameStateDyn) rowOne

        rowTwoE :: Event t GameState <-
            divClass "level" $ do
                leftmost <$> mapM (planetButton gameStateDyn) rowTwo

        rowThreeE :: Event t GameState <-
            divClass "level" $ do
                leftmost <$> mapM (planetButton gameStateDyn) rowThree
 
        return $ leftmost [rowOneE, rowTwoE, rowThreeE]

planetButton :: forall m t .
                ( DomBuilder t m
                , PostBuild t m
                , MonadFix m
                , MonadHold t m
                , MonadIO m)
                => Dynamic t GameState -> PlanetName -> m (Event t GameState)
planetButton gameStateDyn planetName = do
    (elt,_) <-
        elClass "section" "section" $ do
            errMsg
            elAttr' "button" buttonAttr $ do
                iconAttrDyn
                el "span" $ do
                    text $ (pack . show) planetName
    let clickE = domEvent Click elt
        currentPlanetE = currentPlanet planetName 
                             <$> tag (current gameStateDyn) clickE
    return currentPlanetE    
--        $ traceEvent "*** Click! *** " 
--        $ (planet,currentPlanet) 
--        <$ domEvent Click elt
    where
        locationDyn = _location <$> gameStateDyn
        isCurrentDyn = (\loc -> loc == planetName) <$> locationDyn
        errMsg      = return ()
        buttonAttr  = ("class" =: buttonClass)
        buttonClass = "button is-rounded is-link is-outlined"
        iconClassDyn = ffor isCurrentDyn $ \case
                           True -> "fa fa-space-shuttle" 
                           False -> "d-none"
        iconAttrDyn  = elClass "div" "icon is-large" $ do
                           elDynClass "i" iconClassDyn $ blank

currentPlanet :: PlanetName -> GameState -> GameState
currentPlanet planetName (GameState {..}) =
    if isNeighbor then moveSucceeds else moveFails
    where
        moveSucceeds = GameState
                         _screenState
                         planetName
                         Nothing
                         _planetMap
                         _credits
        moveFails    = GameState
                         _screenState
                         _location
                         (Just (planetName, NotNeighbor))
                         _planetMap
                         _credits
        isNeighbor = elem planetName neighbors
        neighbors = case (lookup _location _planetMap) of
                        Nothing -> mempty
                        (Just l) -> _neighbors l
               
                
{-
displayAllPlanets :: forall m t .
                    ( DomBuilder t m
                    , PostBuild t m
                    , MonadFix m
                    , MonadHold t m
                    , MonadIO m)
                    => GameState
                    -> m (Event t GameState)
displayAllPlanets currentState = do
    let (MapFormatting {..}) = formatMap currentState
--    liftIO $ putStrLn "Hello!"
    divClass "section" $ do
        rowOneE :: Event t (PlanetName, GameState) <-
            divClass "level" $ do
                leftmost <$> mapM (planetButton currentState) _rowOne


        rowTwoE :: Event t (PlanetName, GameState) <-
            divClass "level" $ do
                leftmost <$> mapM (planetButton currentState) _rowTwo

        rowThreeE :: Event t (PlanetName, GameState) <-
            divClass "level" $ do
                leftmost <$> mapM (planetButton currentState) _rowThree

        let newPlanetE      = traceEvent "*** New Planet *** "
                                  $ leftmost [rowOneE, rowTwoE, rowThreeE]
            updateLocationE = traceEvent "*** AFter New Planet ***" $ updateLocation <$> newPlanetE
        return updateLocationE
-}
updateLocation :: (PlanetName, GameState) -> GameState
updateLocation (pname, gstate@(GameState {..})) =
    case _error of
      (Just (_, NotNeighbor)) -> gstate
      _                           -> gstate {_location = pname}

{-
planetButton :: forall m t .
                ( DomBuilder t m
                , PostBuild t m
                , MonadFix m
                , MonadHold t m
                , MonadIO m)
                => GameState
                -> PlanetName
                -> m (Event t (PlanetName, GameState))
planetButton g@(GameState {..}) planet = do
    liftIO
        $ putStrLn (("Planet " <> (show planet)) <> ("PlanetButton " <> (show g)))
    (elt,_) <-
        elClass "section" "section" $ do
            errMsg
            elAttr' "button" buttonAttr $ do
                iconAttr
                el "span" $ do
                    text $ (pack . show) planet
    return $ traceEvent "*** Click! *** " $(planet,currentPlanet) <$ domEvent Click elt
        where
            isCurrent = _location == planet
            errMsg
                | isBadMove = divClass "has-text-danger" $
                                   text "Can't get there from here"
                | otherwise  = return ()

            isBadMove = case _error of
                            (Just (p,e)) -> (e == NotNeighbor)
                                              && (p == planet)

                            Nothing      -> False

            currentPlanet
               | isNeighbor = GameState
                                _screenState
                                planet
                                Nothing
                                _planetMap
                                _credits
               | otherwise  = GameState
                                _screenState
                                _location
                                (Just (planet, NotNeighbor))
                                _planetMap
                                _credits
            buttonAttr = ("class" =: (buttonClass <> active)) <> neighborAttr
            (iconClass,active)
              | isCurrent = (iClass, current')
              | otherwise = ("d-none", mempty)
            iconAttr
              | isBadMove = return ()
              | otherwise  = elClass "div" "icon is-large" $ do
                                 elClass "i" iconClass $ blank
            neighbors :: [PlanetName]
            neighbors = case (lookup _location _planetMap) of
                            Nothing -> mempty
                            (Just l) -> _neighbors l
            isNeighbor = elem planet neighbors
            neighborAttr :: Map Text Text
            neighborAttr
              | isCurrent = mempty
              | otherwise = if isNeighbor
                             then ("style" =: "border: 5px solid #0000ff;")
                             else mempty
            iClass      = "fa fa-space-shuttle"
            buttonClass = "button is-rounded is-link is-outlined"
            current'    = mempty
-}

formatMap :: GameState -> MapFormatting
formatMap GameState{..} = MapFormatting rowOne rowTwo rowThree 
marketPanel :: forall m t .
                   ( DomBuilder t m
                   , PostBuild t m
                   , MonadFix m
                   , MonadHold t m
                   , MonadIO m)
                   => Dynamic t GameState -> m (Event t GameState)
marketPanel gameStateDyn = do
    updatedScreenE <- divClass "level-left" $ do
        divClass "level-left" $ do
            (elt,_) <- elClass' "button" panelButton $ do
                text "Planet Travel"
            return $ traceEvent "MarketClick" $ Travel <$ domEvent Click elt
    divClass "level-right" $ do
        text "You can buy or sell these perfectly legal wares."

    let gameStateB = current gameStateDyn
        updatedGameStateE = attachWith
                                updateScreen
                                gameStateB
                                updatedScreenE
    return updatedGameStateE

panelButton :: Text
panelButton = "button is-rounded is-link is-outlined"


      
