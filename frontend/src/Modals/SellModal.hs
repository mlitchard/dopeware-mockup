{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}

module Modals.SellModal where

import Prelude                hiding (lookup)
import Control.Monad                 (join)
import Control.Monad.Fix             (MonadFix)
import Control.Monad.IO.Class        (MonadIO)
import Data.Map.Strict
import Data.Maybe                    (fromMaybe)
import Data.Maybe                    (fromJust) -- In a demo? Okay! 
                                                -- In production? No Way!
import Data.Text
import JavaScript
import Reflex
import Reflex.Dom

import Schema

sellModal :: forall m t .
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
sellModal Nothing = return never
sellModal (Just (GameState _ _ _ Nothing  _ _ _ _)) = return never 
sellModal gs@(Just (GameState _ loc _ (Just resName) _ pmap credits _)) = do
    let title = ("Buying " <> (pack . show $ resName))
    amountE :: Event t PInt <- divClass "modal is-active" $ do
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
                    divClass "level-left" $ do
                        text "cost per unit:"
                    divClass "level-right" $ do
                        text $ (pack . show . fromPInt) resourcePrice
                divClass "level" $ do
                    divClass "level-left" $ 
                        text "Credits:"
                    divClass "level-right" $ 
                        text $ (pack . show) credits
                divClass "level" $ do
                    elAttr "output" bubbleAttr $ blank 
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
                let finalAmountE :: Event t PInt
                    finalAmountE = fmap toPInt $
                        fmapMaybe id $ tag (current amountDyn) clickE
                return $ finalAmountE
    let updatePayload :: Maybe (Resource,Planet)
        updatePayload = (,) <$> mResource <*> mPlanet
        res :: Event t (Maybe GameState)
        res = (updateInventory resName updatePayload gs) <$> amountE
    return res
    where
        
        bubbleAttr :: Map Text Text
        bubbleAttr = ("class" =: "bubble") <> ("id" =: "buy-bubble") 
        rangeWrapAttr = ("class" =: "range-wrap") <> ("id" =: "buy-range-wrap")

        rangeAttr :: Map Text Text
        rangeAttr =
            ("class" =: "range") 
                <> ("type"  =: "range") 
                <> ("id"    =: "buy-range")
                <> ("value" =: rangeAvg)
                <> ("min"   =: "1")
                <> ("max"   =: maxRange)

        maxCost :: PInt 
        maxCost = resourceAmount * resourcePrice
        maxRange, rangeAvg :: Text
        (maxRange,rangeAvg) =
            let mr = div (fromPInt maxCost) (fromPInt credits)
                avg' = (pack . show) $ mr `div` 2
                mr' = (pack . show) mr
            in (mr',avg')
       
        resourceAmount :: PInt
        resourceAmount = fromMaybe zero $ _count <$> mResource
        resourcePrice :: PInt
        resourcePrice = fromMaybe zero $ _currentPrice <$> mResource
        mResource :: Maybe Resource 
        mResource = (join . join) $ lookup resName <$> mResourceMap
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
                     -> PInt
                     -> Maybe GameState
updateInventory _ Nothing _ _  = Nothing
updateInventory _ _ Nothing _  = Nothing
updateInventory _rname (Just (resource, planet)) (Just gstate) amount = 
    Just updatedGS
    where
        currentPlanetName :: PlanetName
        currentPlanetName     = _location gstate
        currentPlanetMap :: PlanetMap
        currentPlanetMap = _planetMap gstate
        currentPlanetRMap = _resourceMap planet 
        currentBuy :: ResourceName -- Danger! Danger! Wil Robinson!
        currentBuy = fromJust $ _buyResource gstate
          
        spend = (_currentPrice resource) * amount  
        updatedPRCount        =   (_count resource) + amount
        updatedPlanetResource = resource {_count = updatedPRCount}
        updatedPlanetResourceMap 
            = insert currentBuy (Just updatedPlanetResource) currentPlanetRMap

        updatedPlanet         = planet {_resourceMap = updatedPlanetResourceMap}
        updatedPlanetMap      = 
            insert currentPlanetName updatedPlanet currentPlanetMap
        currentInventory  = _inventory gstate
        currentRInventory = fromMaybe zero $ lookup currentBuy currentInventory
        updatedRInventory = amount - currentRInventory
        updatedCredits   = (_credits gstate) + spend      
        updatedInventory = insert currentBuy updatedRInventory currentInventory
        updatedGS = gstate { 
                        _buyResource = Nothing
                      , _planetMap   = updatedPlanetMap
                      , _credits     = updatedCredits
                      , _inventory   = updatedInventory
                    }
