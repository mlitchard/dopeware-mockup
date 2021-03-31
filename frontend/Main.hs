{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import Control.Monad.Fix
import Data.Text hiding (map)
import Reflex.Dom

import DopeWars
import Schema

main :: IO ()
main = mainWidget $ do
           evtE <- getPostBuild
           let initE = initGameState <$ evtE
           dopeWars initE
           
