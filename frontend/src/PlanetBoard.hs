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

module PlanetBoard where

import Prelude hiding (lookup)

import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Map.Strict
import Data.Text hiding (map,empty)

import Reflex.Dom

import Panels
import Schema

travelBoard :: forall m t .
                   ( Reflex t
                   , Monad m
                   , MonadFix m
                   , MonadHold t m
                   , MonadIO m
                   , MonadIO (Performable m)
                   , HasJSContext (Performable m)
                   , DomBuilder t m
                   , DomBuilderSpace m ~ GhcjsDomSpace
                   , PerformEvent t m
                   , TriggerEvent t m
                   , PostBuild t m)
                   => Dynamic t GameState -> m (Event t GameState)
travelBoard gameStateDyn = do
    boardSwitchE <- divClass "level" $ do
        travelPanel gameStateDyn
    planetSwitchE <- divClass "level" $ do
        travelMap gameStateDyn
    return $ leftmost [boardSwitchE, planetSwitchE]

travelMap :: forall m t .
                 ( Reflex t
                 , Monad m
                 , MonadFix m
                 , MonadHold t m
                 , MonadIO m
                 , MonadIO (Performable m)
                 , HasJSContext (Performable m)
                 , DomBuilder t m
                 , DomBuilderSpace m ~ GhcjsDomSpace
                 , PerformEvent t m
                 , TriggerEvent t m
                 , PostBuild t m)
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
                    ( Reflex t
                    , Monad m
                    , MonadFix m
                    , MonadHold t m
                    , MonadIO m
                    , MonadIO (Performable m)
                    , HasJSContext (Performable m)
                    , DomBuilder t m
                    , DomBuilderSpace m ~ GhcjsDomSpace
                    , PerformEvent t m
                    , TriggerEvent t m
                    , PostBuild t m)
                    => Dynamic t GameState 
                    -> PlanetName 
                    -> m (Event t GameState)
planetButton gameStateDyn planetName = do
    (elt,_) <-
        elClass "section" "section" $ do
            elDynAttr' "button" buttonAttrDyn $ do
                iconAttrDyn
                el "span" $ do
                    text $ (pack . show) planetName
    let clickE = domEvent Click elt
        cpPayloadDyn   = ffor2 gameStateDyn isNeighborDyn (,)
        currentPlanetE = currentPlanet planetName 
                             <$> tag (current cpPayloadDyn) clickE
    return currentPlanetE    
    where
        isNeighborDyn = isNeighbor planetName <$> gameStateDyn
        locationDyn = _location <$> gameStateDyn
        isCurrentDyn = (\loc -> loc == planetName) <$> locationDyn
        neighborAttrDyn = neighborAttr <$> isNeighborDyn
        buttonClassDyn  = constDyn ("class" =: buttonClass) -- <> neighborAttr
        buttonAttrDyn = buttonClassDyn <> neighborAttrDyn
        
        buttonClass = "button is-rounded is-link is-outlined"
        iconClassDyn = ffor isCurrentDyn $ \case
                           True -> "fa fa-space-shuttle" 
                           False -> "d-none"
        iconAttrDyn  = elClass "div" "icon is-large" $ do
                           elDynClass "i" iconClassDyn $ blank

isBadMove :: PlanetName -> GameState -> Bool
isBadMove planetName (GameState {..}) = 
    case _error of
        (Just (p,e)) -> (e == NotNeighbor) && (p == planetName)
        Nothing      -> False

neighborAttr :: Bool -> Map Text Text
neighborAttr isNeighbor' = if isNeighbor'
                            then ("style" =: "border: 5px solid #0000ff;")
                            else mempty
isNeighbor :: PlanetName -> GameState -> Bool
isNeighbor planetName (GameState {..}) = elem planetName neighbors
    where
        neighbors = 
            case (lookup _location _planetMap) of
                Nothing  -> mempty
                (Just l) -> _neighbors l
        
currentPlanet :: PlanetName -> (GameState, Bool) -> GameState
currentPlanet planetName ((GameState {..}), isNeighbor') =
    if isNeighbor' then moveSucceeds else moveFails
    where
        moveSucceeds = GameState
                         _screenState
                         planetName
                         Nothing
                         _sellResource
                         Nothing
                         _planetMap
                         _credits
                         _inventory
        moveFails    = GameState
                         _screenState
                         _location
                         Nothing
                         _sellResource
                         (Just (planetName, NotNeighbor))
                         _planetMap
                         _credits
                         _inventory
                
updateLocation :: (PlanetName, GameState) -> GameState
updateLocation (pname, gstate@(GameState {..})) =
    case _error of
      (Just (_, NotNeighbor)) -> gstate
      _                           -> gstate {_location = pname}



      
