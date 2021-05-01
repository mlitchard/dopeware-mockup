{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}

module Modals.Modal where

import Prelude                hiding (lookup)
import Control.Monad.Fix             (MonadFix)
import Control.Monad.IO.Class        (MonadIO)
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
                 => (Maybe GameState -> m (Event t (Maybe GameState)))
                 -> Event t GameState -> m (Event t GameState)
modal f gameStateE = do
    knot go
    where
        go :: Event t (Maybe GameState) 
                  -> m (Event t (Maybe GameState), Event t GameState)
        go clickE = do
            let showE :: Event t (m (Event t (Maybe GameState)))
                showE = leftmost [ (f <$> Just <$> gameStateE)
                                 , pure never <$ clickE ]
            resDyn :: Dynamic t (Event t (Maybe GameState))
                <- widgetHold (pure never) showE
            let gameStateE' :: Event t (Maybe GameState)
                gameStateE' = switchPromptlyDyn resDyn
                outCloseE :: Event t GameState
                outCloseE = fmapMaybe id gameStateE'
            return (gameStateE', outCloseE)

