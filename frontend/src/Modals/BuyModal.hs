{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}

module Modals.BuyModal where

import Prelude                hiding (lookup)
import Control.Monad                 (join)
import Control.Monad.Fix             (MonadFix)
import Control.Monad.IO.Class        (MonadIO)
import Data.Map.Strict
import Data.Maybe                    (fromMaybe)
import Data.Text
import JavaScript
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
buyModal (Just (GameState _ _ Nothing _ _ _ _)) = return never 
buyModal gs@(Just (GameState _ loc (Just resourceName) _ pmap credits _)) = do
    let title = ("Buying " <> (pack . show $ resourceName))
    mAmountE :: Event t (Maybe Int) <- divClass "modal is-active" $ do
        divClass "modal-background" $ blank
        divClass "modal-card" $ do
            elClass "header" "modal-card-head" $ do
                elClass "p" "modal-card-title" $
                    text title
                elClass "button" "delete" $ blank    
            elClass "section" "modal-card-body" $ do
                divClass "level" $ do
                    divClass "level-left" $ 
                        text "Amount Available:"
                    divClass "level-right" $ 
                        text $ (pack . show) resourceAmount
                divClass "level" $ do
                    divClass "level-left" $ 
                        text "Credits:"
                    divClass "level-right" $ 
                        text $ (pack . show) credits
                inputE <- elAttr "div" rangeWrapAttr $ do
                    (elt,_) <- elAttr' "input" rangeAttr $ blank
                    _ <- elAttr "output" bubbleAttr $ blank
                    return $ () <$ domEvent Input elt
                (elt,_) <- elClass' "button" "button" $ text "go"
                amountE ::  Event t (Maybe Int) 
                    <- performEvent $ setBubble <$ inputE
                let clickE = () <$ domEvent Click elt
                amountDyn' <- holdDyn Nothing amountE
                let amountDyn = traceDyn "amount test" amountDyn'
                return $ tag (current amountDyn) clickE
    let updatePayload :: Maybe (Resource,Planet)
        updatePayload = (,) <$> mResource <*> mPlanet
        amountE :: Event t Int
        amountE = fmapMaybe id mAmountE
        res :: Event t (Maybe GameState)
        res = (updateInventory resourceName updatePayload gs) <$> amountE
    return res
    where
        
        bubbleAttr :: Map Text Text
        bubbleAttr = ("class" =: "bubble") <> ("id" =: "buy-bubble") 

        rangeWrapAttr :: Map Text Text 
        rangeWrapAttr = ("class" =: "range-wrap") <> ("id" =: "buy-range-wrap")

        rangeAttr :: Map Text Text
        rangeAttr =
            ("class" =: "range") 
                <> ("type" =: "range") 
                <> ("id"   =: "buy-range")
                <> ("min"  =: "20")
                <> ("max"  =: "900")
        resourceAmount :: PInt
        resourceAmount = fromMaybe zero $ _count <$> mResource
        mResource :: Maybe Resource 
        mResource = (join . join) $ lookup resourceName <$> mResourceMap
        mResourceMap :: Maybe (Map ResourceName (Maybe Resource))
        mResourceMap = _resourceMap <$> mPlanet
        mPlanet :: Maybe Planet
        mPlanet = lookup loc pmap
{-
updatePayload :: Maybe Resource -> Maybe Planet -> Maybe (Resource, Planet)
updatePayload (Just res) (Just planet) = Just (res, planet)
updatePayload _          _             = Nothing
-} 
updateInventory :: ResourceName 
                     -> Maybe (Resource, Planet) 
                     -> Maybe GameState
                     -> Int
                     -> Maybe GameState
updateInventory _ Nothing _ _ = Nothing
updateInventory _rname (Just (_resource, _planet)) gstate _amount = gstate 
