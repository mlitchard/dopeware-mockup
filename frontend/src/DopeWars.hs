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
-- import Data.Map.Strict
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
                divClass "level" $ do
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
    let screenE :: Event t (m (Event t ScreenState))
        screenE = ffor (_screenState <$> gameStateE) $ \case
                      Travel -> travelTopPanel
                      Market -> marketTopPanel
    updatedScreenDyn :: Dynamic t (Event t ScreenState) <- 
        widgetHold (return never) screenE
    let updatedScreenE = traceEvent "switchPromptlyDyn" $ switchPromptlyDyn updatedScreenDyn
    mGameStateDyn <- holdDyn Nothing $ Just <$> gameStateE
    let mGameStateB = current mGameStateDyn
        updatedGameStateE = attachWithMaybe 
                                updateScreen 
                                mGameStateB
                                updatedScreenE        
    return updatedGameStateE

updateScreen :: Maybe GameState -> ScreenState -> Maybe GameState
updateScreen Nothing _                 = Nothing
updateScreen (Just gState) screenState =
    Just $ gState {_screenState = screenState}

travelTopPanel :: forall m t .
                      ( DomBuilder t m
                      , PostBuild t m
                      , MonadFix m
                      , MonadHold t m
                      , MonadIO m)
                      => m (Event t ScreenState)
travelTopPanel = do
    divClass "level-left" $ do
        text "Click on a highlighted planet to travel there"
    divClass "level-right" $ do
        (elt,_) <- elClass' "button" panelButton $ do
            text "go to market"
        return $ traceEvent "MarketClick" $ Market <$ domEvent Click elt 

marketTopPanel :: forall m t .
                      ( DomBuilder t m
                      , PostBuild t m
                      , MonadFix m
                      , MonadHold t m
                      , MonadIO m)
                      => m (Event t ScreenState)
marketTopPanel = do
    boardChangeE <- divClass "level-left" $ do
        (elt,_) <- elClass' "button" panelButton $ do
            text "Planet Travel"
        return $ Travel <$ domEvent Click elt
    divClass "level-right" $ do
        text "You can buy or sell these perfectly legal wares."
    return boardChangeE

panelButton :: Text
panelButton = "button is-rounded is-link is-outlined"


      
