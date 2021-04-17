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

import Reflex.Dom

import Panels
import PlanetBoard
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



      
