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

module Panels where

import Prelude hiding (lookup)

import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Text hiding (map,empty)

import Reflex.Dom

import Schema

travelPanel :: forall m t .
                   ( DomBuilder t m
                   , PostBuild t m
                   , MonadFix m
                   , MonadHold t m
                   , MonadIO m)
                   => Dynamic t GameState -> m (Event t GameState)
travelPanel gameStateDyn = do
    let (msgClassDyn,msgTextDyn) =  
            splitDynPure $ ffor (_error <$> gameStateDyn) $ \case
                  (Just (_,NotNeighbor)) -> (errClass',errMsg')
                  _                      -> (promptClass',promptMsg')
    divClass "level-left" $ do
        elDynClass "div" msgClassDyn $ dynText msgTextDyn
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
    where  
        (errClass',errMsg')
            = ("has-text-danger", "You can't get there from here. Select a highlighted planet")
        (promptClass',promptMsg') 
            = ("has-text-bold"
              , "Click on a highlighted planet to travel there")

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


      
