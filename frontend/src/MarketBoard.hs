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

module MarketBoard where

import Prelude hiding (lookup)

import           Control.Monad (join)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Map.Strict
import           Data.Maybe (fromMaybe)
import           Data.Text hiding (map,empty)
import qualified Data.Text as T
import           Reflex.Dom

import           InventoryTable hiding (showAvailability)
import           Modals.BuyModal
import           Panels
import           Schema

marketBoard :: forall m t .
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
marketBoard gameStateDyn = do
    boardSwitchE <- divClass "level" $ do
        marketPanel gameStateDyn
    tableActionE <- divClass "level" $ do
        mtable <- marketTable gameStateDyn
        _ <- inventoryTable gameStateDyn
        return $ leftmost [mtable]
    return $ leftmost [boardSwitchE, tableActionE]

marketTable :: forall m t .
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
marketTable gameStateDyn = do
    divClass "section" $ do
        rowOneE :: Event t GameState <-
            divClass "columns" $ do
                leftmost <$> mapM (marketCell gameStateDyn) marketRowOne

        rowTwoE :: Event t GameState <-
            divClass "columns" $ do
                leftmost <$> mapM (marketCell gameStateDyn) marketRowTwo

        rowThreeE :: Event t GameState <-
            divClass "columns" $ do
                leftmost <$> mapM (marketCell gameStateDyn) marketRowThree

        return $ leftmost [rowOneE, rowTwoE, rowThreeE] 

marketCell :: forall m t .
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
marketCell gameStateDyn resourceName = do
    divClass "column" $ do
        divClass "card" $ do
            gstateE <- elClass "header" "card-header" $ do
                divClass "level" $ do
                    divClass "level-left" $ do
                        text (pack . show $ resourceName)
                    gameStateE <- divClass "level-right" $ do
                        clickE <- buyButton maybeResourceDyn
                        let gameStateE = tag (current gameStateDyn) clickE
                        return $ buyingUpdate resourceName <$> gameStateE
                    modal gameStateE
            _ <- divClass "card-content" $ do
                     showAvailability gameStateDyn maybeResourceDyn resourceName
            return gstateE
    where
        locationDyn = _location <$> gameStateDyn
        planetMapDyn = _planetMap <$> gameStateDyn

        planetDyn :: Dynamic t (Maybe Planet)
        planetDyn = lookup <$> locationDyn <*> planetMapDyn

        resourcesDyn :: Dynamic t (Map ResourceName (Maybe Resource))
        resourcesDyn = fromMaybe empty <$> (fmap . fmap) _resourceMap planetDyn

        maybeResourceDyn :: (Dynamic t (Maybe Resource))
        maybeResourceDyn = join <$> lookup resourceName <$> resourcesDyn

buyingUpdate :: ResourceName -> GameState -> GameState
buyingUpdate resName gstate = gstate {_buyResource = Just resName}
 
buyButton :: forall m t .
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
                 => Dynamic t (Maybe Resource)
                 -> m (Event t ())
buyButton maybeResourceDyn = do
    (elt,_) <- elDynAttr' "button" buttonAttrDyn $ text "Buy"
    return $ () <$ domEvent Click elt
    where
        buttonAttr    = "class" =: buttonClass
        disabledAttr  = "disabled" =: "disabled"
        buttonClass   = "button is-rounded is-success is-outlined"
        buttonAttrDyn = ffor maybeResourceDyn $ \case 
                             (Just _) -> buttonAttr
                             Nothing  -> buttonAttr <> disabledAttr

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
                        -> Dynamic t (Maybe Resource)
                        -> ResourceName 
                        -> m (Event t GameState)
showAvailability _gameStateDyn maybeResourceDyn _resourceName = do
    let availabilityDyn = ffor maybeResourceDyn $ \case
                              Nothing         -> "Unavailable"
                              (Just resource) -> showAmount resource
        (priceClassDyn, priceDyn) = splitDynPure $ ffor maybeResourceDyn $ \case
                                        Nothing -> ("d-none", T.empty)
                                        (Just resource) -> (,) 
                                                             "level" 
                                                             (showPrice resource)
    divClass "level" $ do
        divClass "level-left" $ do
            text "availability"
        divClass "level-right" $ do
            dynText availabilityDyn
    elDynClass "div" priceClassDyn $ do
        divClass "level-left" $ do
            text "price" 
        divClass "level-right" $ do
            dynText priceDyn
    return never
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
showAmount :: Resource -> Text
showAmount = pack . show . fromPInt . _count

showPrice :: Resource -> Text
showPrice = pack . show . fromPInt  . _currentPrice 
