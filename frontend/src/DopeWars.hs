{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module DopeWars where

import Control.Monad.Fix
import Data.Text hiding (map)
import Reflex.Dom

import Schema

rowOne   = [Arrakis, Minbar, Tatooine]
rowTwo   = [CentauriPrime, Vulcan, Dantooine]
rowThree = [Mongo, Terra, Pluto]

dopeWars :: forall m t .
                ( DomBuilder t m
                , PostBuild t m
                , MonadFix m
                , MonadHold t m)
                => Event t (GameState) -> m ()
dopeWars initE = do
    _ <- mfix go
    return ()
    where
        go :: Event t GameState -> m (Event t GameState)
        go newStateE = do
            let stateE = traceEvent "stateE" $ leftmost [newStateE, initE]
            divClass "section" $ do
                displayBoard stateE

displayBoard :: forall m t .
                    ( DomBuilder t m
                    , PostBuild t m
                    , MonadFix m
                    , MonadHold t m)
                    => Event t GameState
                    -> m (Event t GameState)
displayBoard displayE = do
    let allDisplayE :: Event t (m (Event t GameState))
        allDisplayE = displayAllPlanets <$> displayE
    displayDyn :: Dynamic t (Event t (GameState)) <-
        widgetHold (return never) allDisplayE
    return $ switchPromptlyDyn displayDyn

displayAllPlanets :: forall m t .
                    ( DomBuilder t m
                    , PostBuild t m
                    , MonadFix m
                    , MonadHold t m)
                    => GameState
                    -> m (Event t GameState)
displayAllPlanets currentState = do
    let (MapFormatting {..}) = formatMap currentState  
    divClass "section" $ do
        rowOneE :: Event t PlanetName <-
            divClass "level" $ do
                leftmost <$> mapM planetButton _rowOne
        rowTwoE :: Event t PlanetName <-
            divClass "level" $ do
                leftmost <$> mapM planetButton _rowTwo
        rowThreeE :: Event t PlanetName <-
            divClass "level" $ do
                leftmost <$> mapM planetButton _rowThree
        let newPlanetE      = leftmost [rowOneE, rowTwoE, rowThreeE]
            updateLocationE = (updateLocation currentState) <$> newPlanetE 
        return updateLocationE

updateLocation :: GameState -> PlanetName -> GameState
updateLocation gstate pname = gstate {_location = pname}

planetButton :: forall m t .
                ( DomBuilder t m
                , PostBuild t m
                , MonadFix m
                , MonadHold t m)
                => (PlanetName,Bool) -> m (Event t PlanetName)
planetButton (planet,isCurrent) = do
    let (iconClass,active) =
            if isCurrent then (iClass, current) else ("d-none", empty)
    (elt,_) <-
        elClass' "button" (buttonClass <> active) $ do
            elClass "div" "icon is-large" $ do
                elClass "i" iconClass $ blank
            el "span" $ do
                text $ (pack . show) planet
    return $ planet <$ domEvent Click elt
        where
            iClass = "fa fa-space-shuttle"
            buttonClass = "button is-rounded is-link is-outlined"
            current = "is-outlined"

formatMap :: GameState -> MapFormatting
formatMap GameState{..} =
    let r1 = fmap (\p -> (p, (_location == p))) rowOne
        r2 = fmap (\p -> (p, (_location == p))) rowTwo
        r3 = fmap (\p -> (p, (_location == p))) rowThree
    in MapFormatting r1 r2 r3

     
