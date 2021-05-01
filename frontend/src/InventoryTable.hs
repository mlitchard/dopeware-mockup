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

module InventoryTable where

import Prelude hiding (lookup)

-- import           Control.Monad (join)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Map.Strict
-- import           Data.Maybe (fromMaybe)
import           Data.Text hiding (map,empty)
-- import qualified Data.Text as T
import           Reflex.Dom

import           Modals.Modal
import           Modals.SellModal
-- import           Panels
import           Schema

inventoryTable :: forall m t .
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
inventoryTable gameStateDyn = do
    divClass "section" $ do
        rowOneE :: Event t GameState <-
            divClass "columns" $ do
                leftmost <$> mapM (inventoryCell gameStateDyn) marketRowOne

        rowTwoE :: Event t GameState <-
            divClass "columns" $ do
                leftmost <$> mapM (inventoryCell gameStateDyn) marketRowTwo

        rowThreeE :: Event t GameState <-
            divClass "columns" $ do
                leftmost <$> mapM (inventoryCell gameStateDyn) marketRowThree

        return $ leftmost [rowOneE, rowTwoE, rowThreeE] 

inventoryCell :: forall m t .
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
                     -> ResourceName 
                     -> m (Event t GameState)
inventoryCell gameStateDyn resourceName = do
    divClass "column" $ do
        divClass "card" $ do
            gstateE <- elClass "header" "card-header" $ do
                divClass "level" $ do
                    divClass "level-left" $ do
                        text (pack . show $ resourceName)
                    gameStateE <- divClass "level-right" $ do
                        clickE <- sellButton amountDyn
                        let gameStateE = tag (current gameStateDyn) clickE
                        return $ sellingUpdate resourceName <$> gameStateE
                    modal sellModal gameStateE
            _ <- divClass "card-content" $ do
                     showAvailability gameStateDyn amountDyn resourceName
            return gstateE
    where
        amountDyn :: Dynamic t (Maybe PInt)
        amountDyn = lookup resourceName <$> _inventory <$> gameStateDyn 
                    

        

sellingUpdate :: ResourceName -> GameState -> GameState
sellingUpdate resName gstate = gstate {_sellResource = Just resName}
 
sellButton :: forall m t .
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
                 => Dynamic t (Maybe PInt)
                 -> m (Event t ())
sellButton amountDyn = do
    (elt,_) <- elDynAttr' "button" buttonAttrDyn $ text "Sell"
    return $ () <$ domEvent Click elt
    where
        buttonAttr    = "class" =: buttonClass
        disabledAttr  = "disabled" =: "disabled"
        buttonClass   = "button is-rounded is-success is-outlined"
        buttonAttrDyn = ffor amountDyn $ \case 
                             (Just (PInt 0)) -> buttonAttr <> disabledAttr
                             Nothing         -> buttonAttr <> disabledAttr
                             _        -> buttonAttr

showAvailability :: forall m t .
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
                        -> Dynamic t (Maybe PInt)
                        -> ResourceName 
                        -> m (Event t GameState)
showAvailability _gameStateDyn mAmountDyn _resourceName = do
    divClass "level" $ do
        divClass "level-left" $ do
            text "availability"
        divClass "level-right" $ do
            dynText showAmountDyn
    return never
    where
        noneTxt = "Not Available"
        showAmountDyn = ffor mAmountDyn $ \case
                            (Just (PInt 0))      -> noneTxt
                            Nothing              -> noneTxt
                            (Just (PInt amount)) -> (pack . show) amount
{-
    where
        locationDyn = _location <$> gameStateDyn
        planetMapDyn = _planetMap <$> gameStateDyn
        planetDyn :: Dynamic t (Maybe Planet)
        planetDyn = lookup <$> locationDyn <*> planetMapDyn
        resourcesDyn :: Dynamic t (Map ResourceName (Maybe Resource)) 
        resourcesDyn = fromMaybe empty <$> (fmap . fmap) _resources planetDyn 
        maybeResourceDyn :: (Dynamic t (Maybe Resource))
        maybeResourceDyn = join <$> lookup resourceName <$> resourcesDyn
-}

