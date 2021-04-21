{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}

module Modals.BuyModal where

import Control.Monad.Fix               (MonadFix)
import Control.Monad.IO.Class          (MonadIO)
import Data.Text
import Reflex
import Reflex.Dom

import Schema
import Utils

modal :: forall m t .
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
                 => Event t GameState -> m (Event t GameState)
modal gameStateE = do
    knot go
    where
        go :: Event t (Maybe GameState) 
                  -> m (Event t (Maybe GameState), Event t GameState)
        go clickE = do
            let showE :: Event t (m (Event t (Maybe GameState)))
                showE = leftmost [ (buyModal <$> Just <$> gameStateE)
                                 , pure never <$ clickE ]
            resDyn :: Dynamic t (Event t (Maybe GameState))
                <- widgetHold (pure never) showE
            let gameStateE' :: Event t (Maybe GameState)
                gameStateE' = switchPromptlyDyn resDyn
                outCloseE :: Event t GameState
                outCloseE = fmapMaybe id gameStateE'
            return (gameStateE', outCloseE)

buyModal :: forall m t .
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
                => Maybe GameState -> m (Event t (Maybe GameState))
buyModal Nothing = return never
buyModal (Just (GameState {..})) = do
    let title = case _buyResource of
                   Just res -> ("Buying " <> (pack . show $ res))
                   Nothing  -> empty
    divClass "modal is-active" $ do
        divClass "modal-background" $ blank
        divClass "modal-card" $ do
            elClass "header" "modal-card-head" $ do
                elClass "p" "modal-card-title" $
                    text title
                elClass "button" "delete" $ blank    
            elClass "section" "modal-card-body" $
                text "modal"
    return never
            
      
            
                 
    
   


